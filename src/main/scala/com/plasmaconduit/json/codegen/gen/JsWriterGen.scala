package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model.{ModelParameterType, ModelParameter, Model}

import treehugger.forest._
import treehuggerDSL._

sealed trait JsWriterGen {
  def generate(model: Model): treehugger.forest.Tree
}

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

  def JsWriterObjectRepGen(ignore: List[String]) = new JsWriterGen {
    private def generateFieldOutput(field: ModelParameter, refName: String): treehugger.forest.Tree = {
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

    override def generate(model: Model): treehugger.forest.Tree = {
      val modelName = model.name.value

      OBJECTDEF(s"${modelName}JsWriter").withParents(symbols.JsWriterType.APPLYTYPE(model.fullyQualifiedName)) := BLOCK(
        DEF("write", symbols.JsValueType).withParams(PARAM("m", model.fullyQualifiedName)).withFlags(Flags.OVERRIDE) := BLOCK(
          symbols.JsObjectClass.APPLY(
            model.parameters.filter(p => !ignore.contains(p.term.value)).map(field => generateFieldOutput(field, "m"))
          )
        )
      )
    }
  }

  def JsWriterParameterRepGen() = new JsWriterGen {
    private def generateModelOutput(field: ModelParameter, refName: String): treehugger.forest.Tree = {
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
        case ModelParameterType(name, _) => REF(refName)
      }

      value
    }

    override def generate(model: Model): treehugger.forest.Tree = {
      val modelName = model.name.value

      OBJECTDEF(s"${modelName}JsWriter").withParents(symbols.JsWriterType.APPLYTYPE(model.fullyQualifiedName)) := BLOCK(
        DEF("write", symbols.JsValueType).withParams(PARAM("m", model.fullyQualifiedName)).withFlags(Flags.OVERRIDE) := BLOCK(
          generateModelOutput(model.parameters.head, "m")
        )
      )
    }
  }

}
