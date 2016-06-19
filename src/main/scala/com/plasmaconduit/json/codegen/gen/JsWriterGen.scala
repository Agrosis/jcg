package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model.{ModelParameterType, ModelParameter, Model}

import scala.reflect.runtime.universe._

sealed trait JsWriterGen {
  def generate(model: Model): Tree
}

object JsWriterGen {

  private def combine(trees: List[Tree]*): List[Tree] = {
    trees.foldLeft(List[Tree]())((b, a) => b ++ a)
  }

  private implicit def treeToListTree[A <% Tree](tree: A): List[Tree] = {
    List(tree)
  }

  private def generateJsObjectMapValues(tree: Tree, typeParameter: ModelParameterType): Tree = typeParameter match {
    case ModelParameterType("Map", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("mapValues")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            generateJsObjectMapValues(Ident(TermName("x")), innerTypeParameters.drop(1).head)
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
            generateJsArrayMap(Ident(TermName("x")), innerTypeParameters.head)
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
            generateJsObjectMapValues(Ident(TermName("x")), innerTypeParameters.drop(1).head)
          )
        )
      )
    }
    case ModelParameterType("List", innerTypeParameters) => {
      Apply(
        Select(tree, TermName("map")),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            generateJsArrayMap(Ident(TermName("x")), innerTypeParameters.head)
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

  private def generateCustomWriters(customWriters: Map[String, Tree]): List[Tree] = {
    customWriters.toList.map {
      case (name, tree) => ValDef(Modifiers(), TermName(name), TypeTree(), tree)
    }
  }

  def JsWriterObjectRepGen(ignore: List[String]) = new JsWriterGen {
    private def generateFieldOutput(field: ModelParameter, refName: String, customWriters: Map[String, Tree]): Tree = {
      val fieldName = field.term.value
      val fieldWriterName = s"${fieldName}Writer"
      val fieldSelect = Select(Ident(TermName(refName)), TermName(fieldName))

      val value: Tree = customWriters.get(fieldWriterName) match {
        case Some(writer) => {
          Apply(
            Select(Ident(TermName(fieldWriterName)), TermName("write")),
            List(fieldSelect)
          )
        }
        case None => {
          field.parameterType match {
            case ModelParameterType("Map", typeParameters) => {
              generateJsObjectMapValues(fieldSelect, typeParameters.drop(1).head)
            }
            case ModelParameterType("List", typeParameters) => {
              generateJsArrayMap(fieldSelect, typeParameters.head)
            }
            case ModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(fieldSelect))
            case ModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(fieldSelect))
            case ModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(fieldSelect))
            case ModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(fieldSelect))
            case ModelParameterType(name, _) => fieldSelect
          }
        }
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
          combine(
            generateCustomWriters(model.customWriters),
            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("write"),
              List(),
              List(List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(TypeName(model.fullyQualifiedName)), EmptyTree))),
              Ident(TypeName("JsValue")),
              Block(
                List(),
                Apply(Ident(TermName("JsObject")), model.parameters.filter(p => !ignore.contains(p.term.value)).map(field => generateFieldOutput(field, "m", model.customWriters)))
              )
            )
          )
        )
      )
    }
  }

  def JsWriterParameterRepGen() = new JsWriterGen {
    private def generateModelOutput(field: ModelParameter, refName: String, customWriters: Map[String, Tree]): Tree = {
      val fieldName = field.term.value
      val fieldWriterName = s"${fieldName}Writer"
      val fieldSelect = Select(Ident(TermName(refName)), TermName(fieldName))

      customWriters.get(fieldWriterName) match {
        case Some(writer) => {
          Apply(
            Select(Ident(TermName(fieldWriterName)), TermName("write")),
            List(fieldSelect)
          )
        }
        case None => {
          field.parameterType match {
            case ModelParameterType("Map", typeParameters) => {
              Apply(Ident(TermName("JsObject")), List(generateJsObjectMapValues(fieldSelect, typeParameters.drop(1).head)))
            }
            case ModelParameterType("List", typeParameters) => {
              Apply(Ident(TermName("JsArray")), List(generateJsArrayMap(fieldSelect, typeParameters.head)))
            }
            case ModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(fieldSelect))
            case ModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(fieldSelect))
            case ModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(fieldSelect))
            case ModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(fieldSelect))
            case ModelParameterType(name, _) => fieldSelect
          }
        }
      }
    }

    override def generate(model: Model): Tree = {
      ModuleDef(
        Modifiers(),
        TermName(s"${model.name.value}JsWriter"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsWriter")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          combine(
            generateCustomWriters(model.customWriters),
            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("write"),
              List(),
              List(List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(TypeName(model.fullyQualifiedName)), EmptyTree))),
              Ident(TypeName("JsValue")),
              Block(
                List(),
                generateModelOutput(model.parameters.head, "m", model.customWriters)
              )
            )
          )
        )
      )
    }
  }

}
