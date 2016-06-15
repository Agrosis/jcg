package com.plasmaconduit.json.codegen

import treehugger.forest._
import definitions._
import treehuggerDSL._

package object generators {

  object symbols {

    val Map = RootClass.newClass("Map")

    def Validation(e: Type, a: Type) = RootClass.newAbstractType("Validation").TYPE_OF(e, a)
    val Success = RootClass.newClass("Success")
    val Failure = RootClass.newClass("Failure")

    def JsonObjectValueExtractorFunction(e: Type, a: Type) = RootClass.newMethod("JsonObjectValueExtractor").APPLYTYPE(e, a)

    val JsValueType = RootClass.newAbstractType("JsValue")
    val JsObjectClass = RootClass.newClass("JsObject")
    val JsStringClass = RootClass.newClass("JsString")
    val JsBooleanClass = RootClass.newClass("JsBoolean")
    val JsLongClass = RootClass.newClass("JsLong")
    val JsFloatClass = RootClass.newClass("JsFloat")
    val JsArrayClass = RootClass.newClass("JsArray")

    val JsWriterType = RootClass.newAbstractType("JsWriter")
    val JsReaderType = RootClass.newAbstractType("JsReader")

    val JsReaderFailureAliasType = RootClass.newAliasType("JsReaderFailure")
  }

}
