package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model._

import scala.reflect.runtime.universe._

sealed trait JsWriterGen[A <: Model] {
  def generate(model: A): Tree
}

object JsWriterGen {

  private def combine(trees: List[Tree]*): List[Tree] = {
    trees.foldLeft(List[Tree]())((b, a) => b ++ a)
  }

  private implicit def treeToListTree[A <% Tree](tree: A): List[Tree] = {
    List(tree)
  }

  private def generateJsValueMap(tree: Tree, typeParameter: ClassModelParameterType, mapFunction: String): Tree = typeParameter match {
    case ClassModelParameterType("Map", innerTypeParameters) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsObject")), List(generateJsValueMap(Ident(TermName("x")), innerTypeParameters.drop(1).head, "mapValues")))
          )
        )
      )
    }
    case ClassModelParameterType("List", innerTypeParameters) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            generateJsValueMap(Ident(TermName("x")), innerTypeParameters.head, "map")
          )
        )
      )
    }
    case ClassModelParameterType("Option", innerTypeParameters) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
            Apply(Ident(TermName("JsOption")), List(generateJsValueMap(Ident(TermName("x")), innerTypeParameters.drop(1).head, "map")))
          )
        )
      )
    }
    case ClassModelParameterType("Long", _) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsLong")), List(Ident(TermName("x"))))))
      )
    }
    case ClassModelParameterType("String", _) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsString")), List(Ident(TermName("x"))))))
      )
    }
    case ClassModelParameterType("Float", _) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsFloat")), List(Ident(TermName("x"))))))
      )
    }
    case ClassModelParameterType("Boolean", _) => {
      Apply(
        Select(tree, TermName(mapFunction)),
        List(Function(List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)), Apply(Ident(TermName("JsBoolean")), List(Ident(TermName("x"))))))
      )
    }
    case ClassModelParameterType(name, _) => tree
  }

  private def generateCustomWriters(customWriters: Map[String, Tree]): List[Tree] = {
    customWriters.toList.map {
      case (name, tree) => ValDef(Modifiers(), TermName(name), TypeTree(), tree)
    }
  }

  def JsWriterObjectRepGen(ignore: List[String], isChild: Boolean) = new JsWriterGen[ClassModel] {
    private def generateFieldOutput(field: ClassModelParameter, refName: String, customWriters: Map[String, Tree]): Tree = {
      val fieldName = field.term
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
            case ClassModelParameterType("Map", typeParameters) => {
              Apply(Ident(TermName("JsObject")), generateJsValueMap(fieldSelect, typeParameters.drop(1).head, "mapValues"))
            }
            case ClassModelParameterType("List", typeParameters) => {
              generateJsValueMap(fieldSelect, typeParameters.head, "map")
            }
            case ClassModelParameterType("Option", typeParameters) => {
              Apply(Ident(TermName("JsOption")), generateJsValueMap(fieldSelect, typeParameters.head, "map"))
            }
            case ClassModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(fieldSelect))
            case ClassModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(fieldSelect))
            case ClassModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(fieldSelect))
            case ClassModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(fieldSelect))
            case ClassModelParameterType(name, _) => fieldSelect
          }
        }
      }

      Apply(Ident(TermName("Tuple2")), List(Literal(Constant(field.term)), value))
    }

    override def generate(model: ClassModel): Tree = {
      val fieldMappings = model.parameters.filter(p => !ignore.contains(p.term)).map(field => generateFieldOutput(field, "m", model.customWriters))

      val withType =
        if (isChild) fieldMappings ++ Apply(Ident(TermName("Tuple2")), List(Literal(Constant("type")), Literal(Constant(model.name))))
        else fieldMappings

      ModuleDef(
        Modifiers(),
        TermName(s"${model.name}JsWriter"),
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
                Apply(Ident(TermName("JsObject")), withType)
              )
            )
          )
        )
      )
    }
  }

  def JsWriterParameterRepGen() = new JsWriterGen[ClassModel] {
    private def generateModelOutput(field: ClassModelParameter, refName: String, customWriters: Map[String, Tree]): Tree = {
      val fieldName = field.term
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
            case ClassModelParameterType("Map", typeParameters) => {
              Apply(Ident(TermName("JsObject")), List(generateJsValueMap(fieldSelect, typeParameters.drop(1).head, "mapValues")))
            }
            case ClassModelParameterType("List", typeParameters) => {
              generateJsValueMap(fieldSelect, typeParameters.head, "map")
            }
            case ClassModelParameterType("Option", typeParameters) => {
              Apply(Ident(TermName("JsOption")), List(generateJsValueMap(fieldSelect, typeParameters.drop(1).head, "map")))
            }
            case ClassModelParameterType("Long", _) => Apply(Ident(TermName("JsLong")), List(fieldSelect))
            case ClassModelParameterType("String", _) => Apply(Ident(TermName("JsString")), List(fieldSelect))
            case ClassModelParameterType("Float", _) => Apply(Ident(TermName("JsFloat")), List(fieldSelect))
            case ClassModelParameterType("Boolean", _) => Apply(Ident(TermName("JsBoolean")), List(fieldSelect))
            case ClassModelParameterType(name, _) => fieldSelect
          }
        }
      }
    }

    override def generate(model: ClassModel): Tree = {
      ModuleDef(
        Modifiers(),
        TermName(s"${model.name}JsWriter"),
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

  def JsWriterTraitGen(children: List[ClassModel], termPackageMap: Map[String, String]) = new JsWriterGen[TraitModel] {
    def generateCases(children: List[ClassModel]): List[CaseDef] = {
      children.map(c => {
        val name = termPackageMap.getOrElse(c.name, c.name)
        CaseDef(
          Bind(TermName("c"), Typed(Ident(termNames.WILDCARD), Ident(TypeName(name)))),
          EmptyTree,
          Apply(
            Select(Ident(TermName(s"${c.name}JsWriter")), TermName("write")),
            List(Ident(TermName("c")))
          )
        )
      })
    }

    override def generate(model: TraitModel): Tree = {
      ModuleDef(
        Modifiers(),
        TermName(s"${model.name}JsWriter"),
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
              Block(
                List(),
                Match(
                  Ident(TermName("m")),
                  generateCases(children)
                )
              )
            )
          )
        )
      )
    }
  }

}
