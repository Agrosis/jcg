package com.plasmaconduit.json.codegen.generators

import com.plasmaconduit.json.codegen.{Model, ModelParameter, ModelParameterType, symbols}
import treehugger.forest._

object JsWriterGen {

  private def generateJsObjectMapValues(tree: treehugger.forest.Tree, typeParameter: ModelParameterType): treehugger.forest.Tree = typeParameter match {
    case ModelParameterType("Map", innerTypeParameters) => {
      tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("x"), innerTypeParameters.drop(1).head)))
    }
    case ModelParameterType("List", innerTypeParameters) => {
      tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("x"), innerTypeParameters.head)))
    }
    case ModelParameterType("Long", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsLongClass.APPLY(REF("x"))))
    case ModelParameterType("String", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsStringClass.APPLY(REF("x"))))
    case ModelParameterType("Float", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsFloatClass.APPLY(REF("x"))))
    case ModelParameterType("Boolean", _) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsBooleanClass.APPLY(REF("x"))))
    case ModelParameterType(name, _) => tree
  }

  private def generateJsArrayMap(tree: treehugger.forest.Tree, typeParameter: ModelParameterType): treehugger.forest.Tree = typeParameter match {
    case ModelParameterType("Map", innerTypeParameters) => {
      tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("x"), innerTypeParameters.drop(1).head)))
    }
    case ModelParameterType("List", innerTypeParameters) => {
      tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("x"), innerTypeParameters.head)))
    }
    case ModelParameterType("Long", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsLongClass.APPLY(REF("x"))))
    case ModelParameterType("String", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsStringClass.APPLY(REF("x"))))
    case ModelParameterType("Float", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsFloatClass.APPLY(REF("x"))))
    case ModelParameterType("Boolean", _) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (symbols.JsBooleanClass.APPLY(REF("x"))))
    case ModelParameterType(name, _) => tree
  }

  def generateModelAssignment(field: ModelParameter, refName: String): treehugger.forest.Tree = {
    val fieldName = field.term.value
    val value: treehugger.forest.Tree = field.parameterType match {
      case ModelParameterType("Map", typeParameters) => {
        symbols.JsObjectClass.APPLY(generateJsObjectMapValues(REF("m").DOT(fieldName), typeParameters.drop(1).head))
      }
      case ModelParameterType("List", typeParameters) => {
        symbols.JsArrayClass.APPLY(generateJsArrayMap(REF("m").DOT(fieldName), typeParameters.head))
      }
      case ModelParameterType("Long", _) => symbols.JsLongClass.APPLY(REF(refName).DOT(fieldName))
      case ModelParameterType("String", _) => symbols.JsStringClass.APPLY(REF(refName).DOT(fieldName))
      case ModelParameterType("Float", _) => symbols.JsFloatClass.APPLY(REF(refName).DOT(fieldName))
      case ModelParameterType("Boolean", _) => symbols.JsBooleanClass.APPLY(REF(refName).DOT(fieldName))
      case ModelParameterType(name, _) => REF(refName).DOT(fieldName)
    }

    TUPLE(LIT(field.term.value), value)
  }

  def generateJsWriterFor(model: Model): treehugger.forest.Tree = {
    val modelName = model.name.value

    OBJECTDEF(s"${modelName}JsWriter").withParents(symbols.JsWriterType.APPLYTYPE(model.getFullyQualifiedName)) := BLOCK(
      DEF("write", symbols.JsValueType).withParams(PARAM("m", model.getFullyQualifiedName)).withFlags(Flags.OVERRIDE) := BLOCK(
        REF(symbols.JsObjectClass).APPLY(
          model.parameters.map(field => generateModelAssignment(field, "m"))
        )
      )
    )
  }

}
