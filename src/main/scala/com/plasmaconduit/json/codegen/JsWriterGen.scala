package com.plasmaconduit.json.codegen

import treehugger.forest._
import treehuggerDSL._

object JsWriterGen {

  private def generateJsObjectMapValues(tree: treehugger.forest.Tree, typeParameter: ModelFieldType): treehugger.forest.Tree = typeParameter match {
    case ModelFieldType("Map", innerTypeParameters) => {
      tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("x"), innerTypeParameters.drop(1).head)))
    }
    case ModelFieldType("List", innerTypeParameters) => {
      tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("x"), innerTypeParameters.head)))
    }
    case ModelFieldType("Long", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsLongClass.APPLY(REF("x"))))
    case ModelFieldType("String", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsStringClass.APPLY(REF("x"))))
    case ModelFieldType("Float", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsFloatClass.APPLY(REF("x"))))
    case ModelFieldType("Boolean", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsBooleanClass.APPLY(REF("x"))))
    case ModelFieldType(name, _) => tree
  }

  private def generateJsArrayMap(tree: treehugger.forest.Tree, typeParameter: ModelFieldType): treehugger.forest.Tree = typeParameter match {
    case ModelFieldType("Map", innerTypeParameters) => {
      tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("x"), innerTypeParameters.drop(1).head)))
    }
    case ModelFieldType("List", innerTypeParameters) => {
      tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("x"), innerTypeParameters.head)))
    }
    case ModelFieldType("Long", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsLongClass.APPLY(REF("x"))))
    case ModelFieldType("String", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsStringClass.APPLY(REF("x"))))
    case ModelFieldType("Float", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsFloatClass.APPLY(REF("x"))))
    case ModelFieldType("Boolean", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsBooleanClass.APPLY(REF("x"))))
    case ModelFieldType(name, _) => tree
  }

  def generateModelAssignment(field: ModelField, refName: String): treehugger.forest.Tree = {
    val fieldName = field.term.value
    val value: treehugger.forest.Tree = field.fieldType match {
      case ModelFieldType("Map", typeParameters) => {
        symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("m").DOT(fieldName), typeParameters.drop(1).head))
      }
      case ModelFieldType("List", typeParameters) => {
        symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("m").DOT(fieldName), typeParameters.head))
      }
      case ModelFieldType("Long", _) => symbols.JsLongClass.APPLY(REF(refName).DOT(fieldName))
      case ModelFieldType("String", _) => symbols.JsStringClass.APPLY(REF(refName).DOT(fieldName))
      case ModelFieldType("Float", _) => symbols.JsFloatClass.APPLY(REF(refName).DOT(fieldName))
      case ModelFieldType("Boolean", _) => symbols.JsBooleanClass.APPLY(REF(refName).DOT(fieldName))
      case ModelFieldType(name, _) => REF(refName).DOT(fieldName)
    }

    TUPLE(LIT(field.term.value), value)
  }

  def generateJsWriterFor(model: Model): treehugger.forest.Tree = {
    val modelName = model.name.value

    OBJECTDEF(s"${modelName}JsWriter").withFlags(Flags.IMPLICIT).withParents(symbols.JsWriterType.APPLYTYPE(modelName)) := BLOCK(
      DEF("write", symbols.JsValueType).withParams(PARAM("m", modelName)).withFlags(Flags.OVERRIDE) := BLOCK(
        REF(symbols.JsObjectClass).APPLY(
          model.fields.map(field => generateModelAssignment(field, "m"))
        )
      )
    )
  }

}
