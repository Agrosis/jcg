package com.plasmaconduit.json

import treehugger.forest._
import definitions._
import treehuggerDSL._

package object codegen {

  object symbols {
    val JsValueType = RootClass.newAbstractType("JsValue")
    val JsWriterType = RootClass.newAbstractType("JsWriter")
    val JsObjectClass = RootClass.newClass("JsObject")
    val JsStringClass = RootClass.newClass("JsString")
    val JsBooleanClass = RootClass.newClass("JsBoolean")
    val JsLongClass = RootClass.newClass("JsLong")
    val JsFloatClass = RootClass.newClass("JsFloat")
    val JsArrayClass = RootClass.newClass("JsArray")
  }

}
