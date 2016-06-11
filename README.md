Json
====

[![Build Status](https://travis-ci.org/plasmaconduit/json.svg?branch=master)](https://travis-ci.org/plasmaconduit/json)

This is a very simple JSON parser with minimal dependencies. The parser itself is
only about 60 lines of code and the code to print json is much less.

Install
-------

Add the following to your `Build.scala`
```
resolvers += "Plasma Conduit Repository" at "http://dl.bintray.com/plasmaconduit/releases",
libraryDependencies += "com.plasmaconduit" %% "json" % "0.21.0"
```

Usage
-----
Parse JSON:
```scala
import com.plasmaconduit.json._

val result: Option[JsValue] = JsonParser.parse("""{"key1": "value", "key2": true}""")
```

Represent JSON objects in Scala:
```scala
import com.plasmaconduit.json._

val verbose = JsObject(
  "key1" -> JsString("value"),
  "key2" -> JsInt(42),
  "key3" -> JsBoolean(true),
  "key4" -> JsArray(JsInt(3), JsBoolean(true))
)

val sugar = JsObject(
  "key1" -> "value",
  "key2" -> 42,
  "key3" -> true,
  "key4" -> List[JsValue](3, true)
)

val moreSugar: JsValue = Map[String, JsValue](
  "key1" -> "value",
  "key2" -> 42,
  "key3" -> true,
  "key4" -> List[JsValue](3, true)
)
```

Convert to native types and collections:
```scala
val nativeList: Option[List[Int]] = JsArray(JsInt(1), JsInt(2), JsInt(3)).as[List[Int]]

val failedConversion = JsArray(JsString("not"), JsString("ints")).as[List[Int]]
```
