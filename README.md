jcg
===

`jcg` is a library that automatically generates `JsWriters` and `JsReaders` from `json` for your models.

Motivation
----------

This tool is meant to be used in conjunction with [json](https://github.com/plasmaconduit/json), which exposes two type classes, `JsWriter` and `JsReader`. These can be used to implement serializers and deserializers to and from JSON, with type safety and an assortment of error values. A trivial model:

```scala
import com.plasmaconduit.json._
import com.plasmaconduit.validation._

final case class PhoneNumber(value: String)

object PhoneNumber {
  implicit object PhoneNumberJsWriter extends JsWriter[PhoneNumber] {
    override def write(p: PhoneNumber): JsValue = JsObject(
      "value" -> p.value
    )
  }

  implicit object PhoneNumberJsReader extends JsReader[PhoneNumber] {
    type JsReaderFailure = PhoneNumberJsReaderError

    sealed trait PhoneNumberJsReaderError
    case object PhoneNumberNotJsonObject extends PhoneNumberJsReaderError
    case object PhoneNumberNumberInvalidError extends PhoneNumberJsReaderError
    case object PhoneNumberNumberMissingError extends PhoneNumberJsReaderError

    val numberExtractor = JsonObjectValueExtractor[String, PhoneNumberJsReaderError](
        key = "number",
        missing = PhoneNumberNumberMissingError,
        invalid = _ => PhoneNumberNumberInvalidError,
        default = None
    )

    override def read(value: JsValue): Validation[PhoneNumberJsReaderError, PhoneNumber] = {
      value match {
        case JsObject(map) => {
          for (number <- numberExtractor(map)) yield PhoneNumber(number)
        }
        case _ => Failure(PhoneNumberNotJsonObject)
      }
    }
  }
}
```

This is a lot of boilerplate, and adding additional fields is time consuming. As your codebase grows with more and more models, writing implementations for these traits becomes fairly repetitive and time consuming. The error values are definitely useful, but it takes a bit of work to create all of them.

Instead of falling back to reflection on-the-fly to serialize/deserialize to and from JSON (thereby giving up type safety and all those error values), we realized that the schema for a `JsReader` or `JsWriter` can more or less be inferred from the declaration of a model. Therefore, given the Scala AST of a model, we can automatically generate a `JsWriter` or `JsReader`.

`jcg` scans your Scala codebase for models and generates code and dumps it to a package of your choice.


Install
-------

Add the following to your `Build.scala`
```
resolvers += "Plasma Conduit Repository" at "http://dl.bintray.com/plasmaconduit/releases",
libraryDependencies += "com.plasmaconduit" %% "jcg-traits" % "0.4.3"
```

Download the latest `jcg` binary that matches the minor version of the `jcg-traits` dependency you downloaded.

Usage
-----

First, specify the models for which you want to generate a `JsReader` and/or `JsWriter` by extending the `GenReader` and/or `GenWriter` trait and choose a reader and/or writer representation (more on this later).

```scala
import com.plasmaconduit.json.codegen.traits._

final case class User(id: Int,
                      username: String,
                      password: String,
                      email: String,
                      items: List[Item]) extends GenWriter

final case class Item(id: Int, name: String) extends GenReader with GenWriter

final case class PhoneNumber(value: String) extends GenReader with GenWriter {
  override val writerRep = GenParameterRep
  override val readerRep = GenParameterRep
}
```

Run the tool in your root directory to generate all the code you don't have to write anymore!

```
java -cp "jcg-0.4.7.jar:." com.plasmaconduit.json.codegen.JsGenerator /path/to/jcg/examples src/main/scala org.company.app json
```

This will recursively find models in the package `org.company.app` create a package `json` with the generated code. Now, you can use the readers and writers wherever you'd like!

```scala
import json.writers.GenJsWriters._
import json.readers.GenJsReaders._

val user: User = ...

// implicit for JsWriter[User] is automatically resolved
val js = JsObject(
  "user" -> user
)

val inputItem: JsValue = ...

// implicit for JsReader[Item] is automatically resolved
val item: Validation[ItemJsReaderError, Item] = inputItem.as[Item]
```

Refer to the `examples/` directory in the root repository for more examples.

Representations
---------------

### GenObjectRep ###

The default representation `jcg` uses for a reader or a writer is `GenObjectRep()`, which treats the model solely as an JSON object. This is reasonable when we have a model with multiple fields, like `User`. You can specify fields to ignore by passing in a List of field names to ignore.

```scala
final case class User(id: Int,
                      username: String,
                      password: String,
                      ssn: String,
                      email: String, items: List[Item]) extends GenWriter {
  val writerRep = GenWriterRep(Ignore("password", "ssn"))
}
```

### GenParameterRep ###

In some cases, we have a single field model that only acts as a wrapper model, like `PhoneNumber`, which wraps a `String`. With `GenParameterRep`, we can choose
to delegate the model representation to the parameter, so the model itself is not represented as a JSON object, but as a JSON string, for example.

```scala
final case class PhoneNumber(value: String) extends GenReader with GenWriter {
  override val readerRep = GenParameterRep
  override val writerRep = GenParameterRep
}
```

An object of type `PhoneNumber` will be correctly serialized and deserialized to and from a `JsString`.

Note: `GenParameterRep` should only be used on models with single fields. Otherwise, `GenObjectRep` should always be used.

Custom readers and writers
--------------------------
For our models, we often need to use a data type that we haven't written, such as `LocalDateTime` in Java 8. We didn't write the type, so we can't specify a `GenReader` or `GenWriter` for those types. Instead, you can write a custom reader/writer that does exactly what you want and specify what field of your model to use it for by declaring a `val` with the field name followed by "Writer" or "Reader":

```scala
final case class Date(date: java.time.LocalDateTime) extends GenWriter {
  override val writerRep = GenParameterRep
  val dateWriter = org.company.app.models.Date.DateLocalTimeWriter
}

object Date {
  object DateLocalTimeWriter extends JsWriter[LocalDateTime] {
    val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override def write(l: LocalDateTime): JsValue = {
      JsString(l.format(dateTimeFormatter))
    }
  }
}
```

This is also a useful way to manipulate field values before writing/reading them.

```scala
final case class Item(id: Int, name: String) extends GenWriter {
  val nameWriter = org.company.app.models.Item.ItemNameJsWriter
}

object Item {
  object ItemNameJsWriter extends JsWriter[String] {
    override def write(a: String): JsValue = {
      JsString(s"item_$a")
    }
  }
}

```

ADT reader/writer generation
----------------------------
It is common to have an algebratic data type for a model, that is, a set of models that are inhabitants of some parent type. For example, we can define an ADT for vehicles:

```scala
sealed trait Vehicle
final case class Car(seats: Int) extends Vehicle
final case class Truck(space: Int) extends Vehicle
final case class Boat(seats: Int) extends Vehicle
```

Even though `Vehicle` isn't a concrete model, we would like to be able to serialize a `Vehicle` as the correct inhabitant. It would also be convenient to deserialize a `Vehicle`, as one of its inhabitants.

To solve this, we include a `type` field in the serialized JSON that specifies which inhabitant the JSON object represents. To specify an ADT for generation:

```scala
sealed trait Vehicle extends GenWriter with GenReader
final case class Car(seats: Int) extends Vehicle with GenWriter with GenReader
final case class Truck(space: Int) extends Vehicle with GenWriter with GenReader
final case class Boat(seats: Int) extends Vehicle with GenWriter with GenReader
```

There are a few things to note when using ADT generation:
1. The inhabitants of the trait must extend the `Gen` traits that the trait does. Otherwise there will be compiler warnings for the generated code.
2. The inhabitants of the trait must have a `GenObjectRep` representation.
3. The inhabitants of the trait should not have a `type` field.

Caveats
-------
`jcg` is not a full-fledged compiler or typechecker, so when specifying fields like `LocalDateTime` or custom readers/writers, try to use the fully qualified name to types, otherwise you will have to resolve these errors in the generate code. We should also be able to serialize/deserialize each inhabitant of `Vehicle` individually without any problems.

If you use a model that will be tracked by `jcg` as a field in another model, its fully qualified name will automatically be determined and used in the generated code for your convenience.

TODO
----
* Implement reader/writer generators for sealed traits.
* Insert comments/warnings.
