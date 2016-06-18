package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model.{ModelParameterType, ModelParameter, Model}

import scala.reflect.runtime.universe._

sealed trait JsWriterGen {
  def generate(model: Model): Tree
}

object JsWriterGen {

  private def generateJsObjectMapValues(tree: Tree, typeParameter: ModelParameterType): Tree = typeParameter match {
    case ModelParameterType("Map", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsObject")), List(generateJsObjectMapValues(Ident(TermName("x")), innerTypeParameters.drop(1).head)))
          )
        )
      )
    }
    case ModelParameterType("List", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsArray")), List(generateJsArrayMap(Ident(TermName("x")), innerTypeParameters.head)))
          )
        )
      )
    }
    case ModelParameterType("Long", _) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsLong")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("String", _) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsString")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("Float", _) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsFloat")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("Boolean", _) => {
      Apply(
        Select(tree, TermName("map")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsBoolean")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType(name, _) => tree
  }

  private def generateJsArrayMap(tree: Tree, typeParameter: ModelParameterType): Tree = typeParameter match {
    case ModelParameterType("Map", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("map")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsObject")), List(generateJsObjectMapValues(Ident(TermName("x")), innerTypeParameters.drop(1).head)))
          )
        )
      )
    }
    case ModelParameterType("List", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsArray")), List(generateJsArrayMap(Ident(TermName("x")), innerTypeParameters.head)))
          )
        )
      )
    }
    case ModelParameterType("Long", _) => {
      Apply(
        Select(tree, TermName("map")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsLong")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("String", _) => {
      Apply(
        Select(Ident(TermName("x")), TermName("map")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsString")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("Float", _) => {
      Apply(
        Select(Ident(TermName("x")), TermName("map")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsFloat")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType("Boolean", _) => {
      Apply(
        Select(Ident(TermName("x")), TermName("map")),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsBoolean")), List(Ident(TermName("x"))))))
      )
    }
    case ModelParameterType(name, _) => tree
  }

  def JsWriterObjectRepGen(ignore: List[String]) = new JsWriterGen {
    private def generateFieldOutput(field: ModelParameter, refName: String): Tree = {
      val fieldName = field.term.value
      val value: Tree = field.parameterType match {
        case ModelParameterType("Map", typeParameters) => {
          Apply(Ident(TermName("JsObject")), List(generateJsObjectMapValues(Select(Ident(TermName(refName)), TermName(fieldName)), typeParameters.drop(1).head)))
        }
        case ModelParameterType("List", typeParameters) => {
          Apply(Ident(TermName("JsArray")), List(generateJsArrayMap(Select(Ident(TermName(refName)), TermName(fieldName)), typeParameters.head)))
        }
        case ModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType(name, _) => Ident(TermName(refName))
      }

      Apply(Select(Ident(TermName("scala")), TermName("Tuple2")), List(Literal(Constant(field.term.value)), value))
    }

    override def generate(model: Model): Tree = {
      ModuleDef(
        Modifiers(),
        TermName(s"${model.name.value}JsWriter"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsWriter")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          List(
            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("write"),
              List(),
              List(List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(TypeName(model.fullyQualifiedName)), EmptyTree))),
              Ident(TypeName("JsValue")),
              Apply(Ident(TermName("JsObject")), model.parameters.filter(p => !ignore.contains(p.term.value)).map(field => generateFieldOutput(field, "m")))
            )
          )
        )
      )
    }
  }

  def JsWriterParameterRepGen() = new JsWriterGen {
    private def generateModelOutput(field: ModelParameter, refName: String): Tree = {
      val fieldName = field.term.value
      field.parameterType match {
        case ModelParameterType("Map", typeParameters) => {
          Apply(Ident(TermName("JsObject")), List(generateJsObjectMapValues(Select(Ident(TermName(refName)), TermName(fieldName)), typeParameters.drop(1).head)))
        }
        case ModelParameterType("List", typeParameters) => {
          Apply(Ident(TermName("JsArray")), List(generateJsArrayMap(Select(Ident(TermName(refName)), TermName(fieldName)), typeParameters.head)))
        }
        case ModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(Select(Ident(TermName(refName)), TermName(fieldName))))
        case ModelParameterType(name, _) => Ident(TermName(refName))
      }
    }

    override def generate(model: Model): Tree = {
      ModuleDef(
        Modifiers(),
        TermName(s"${model.name.value}JsWriter"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsWriter")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          List(
            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("write"),
              List(),
              List(List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(TypeName(model.fullyQualifiedName)), EmptyTree))),
              Ident(TypeName("JsValue")),
              generateModelOutput(model.parameters.head, "m")
            )
          )
        )
      )
    }
  }

}
