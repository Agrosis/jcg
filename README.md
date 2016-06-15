json-codegen
============

Motivation
----------

This tool is meant to be used in conjunction with [json](https://github.com/plasmaconduit/json), which exposes two important traits, `JsWriter` and `JsReader`,
which can be used to implement a serializer and deserializer to and from JSON. Here is an example for a trivial model.

```scala
import com.plasmaconduit.json._
import com.plasmaconduit.validation._

final case class PhoneNumber(value: String)

object PhoneNumber {
  implicit object PhoneNumberJsWriter extends JsWriter[PhoneNumber] {
    override def write(p: PhoneNumber): JsValue = JsObject(
      "value" -> value
    )
  }

  implicit object PhoneNumberJsReader extends JsReader[PhoneNumber] {
    type JsReaderFailure = PhoneNumberJsReaderError

    sealed trait PhoneNumberJsReaderError
    case object PhoneNumberNotJsonObject extends PhoneNumberJsReaderError
    case object PhoneNumberNumberInvalidError extends PhoneNumberJsReaderError
    case object PhoneNumberNumberMissingError extends PhoneNumberJsReaderError

    val numberExtractor = JsonObjectValueExtractor[String, PhoneNumberJsReaderError](key = "number", missing = PhoneNumberNumberMissingError, invalid = _ => PhoneNumberNumberInvalidError)

    override def read(value: JsValue): Validation[PhoneNumberJsReaderError, org.company.app.models.phone.PhoneNumber] = {
      value match {
        case JsObject(map) => {
          for (number <- numberExtractor(map)) yield org.company.app.models.phone.PhoneNumber(number = number)
        }
        case _ => Failure(PhoneNumberNotJsonObject)
      }
    }
  }
}
```

It is obvious that this is quite a bit of boilerplate to get a `JsWriter` or a `JsReader` working, but even more importantly, adding additional fields is time consuming.
But, as your codebase grows with more and more models, writing implementations for these traits becomes fairly repetitive and time consuming. All those error objects
are fairly handy, but are quite irritating to write for all of the fields.

Instead of falling back to reflection on-the-fly to serialize/deserialize to and from JSON (thereby giving up type safety), we realized that all the components of a `JsWriter`
or a `JsReader` can be directly inferred from the names and types of the parameters of a model. Therefore, given a Scala AST of a model, we can automatically generate code for them
to do the job.

`json-codegen` scans your Scala codebase for models and generates code (`JsWriters` and `JsReaders`) and dumps it to a directory of your choice.


Install
-------

Add the following to your `Build.scala`
```
resolvers += "Plasma Conduit Repository" at "http://dl.bintray.com/plasmaconduit/releases",
libraryDependencies += "com.plasmaconduit" %% "json-codegen-traits" % "0.2.0"
```

Usage
-----

First, specify the models for which you want to generate a `JsReader` or `JsWriter`:

```scala
import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

final case class User(id: Int, username: String, password: String, email: String) extends GenWriter

final case class Item(id: Int, name: String) extends GenReader with GenWriter

final case class PhoneNumber(value: String) extends GenReader
```

Run the tool and generate code.

TODO
----
* Find a good way to run the tool.
* Implement reader/writer generators for sealed traits.
