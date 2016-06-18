package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model.{ModelDefaultParameterValues, ModelParameterType, ModelParameter, Model}

import scala.reflect.runtime.universe._

sealed trait JsReaderGen {
  def generate(model: Model): Tree
}

object JsReaderGen {

  private def combine(trees: List[Tree]*): List[Tree] = {
    trees.foldLeft(List[Tree]())((b, a) => b ++ a)
  }

  private implicit def treeToListTree[A <% Tree](tree: A): List[Tree] = {
    List(tree)
  }

  private def getClassType(fieldType: ModelParameterType, termPackageMap: Map[String, String]): Tree = fieldType match {
    case ModelParameterType("Map", innerTypeParameters) => {
      TypeApply(Ident(TermName("Map")), List(getClassType(innerTypeParameters.head, termPackageMap: Map[String, String]), getClassType(innerTypeParameters.drop(1).head, termPackageMap: Map[String, String])))
    }
    case ModelParameterType("List", innerTypeParameters) => {
      TypeApply(Ident(TermName("List")), List(getClassType(innerTypeParameters.head, termPackageMap: Map[String, String])))
    }
    case ModelParameterType("Long", _) => Ident(TermName("Long"))
    case ModelParameterType("String", _) => Ident(TermName("String"))
    case ModelParameterType("Float", _) => Ident(TermName("Float"))
    case ModelParameterType("Boolean", _) => Ident(TermName("Boolean"))
    case ModelParameterType(typeName, _) => {
      termPackageMap.get(typeName) match {
        case Some(fullyQualified) => Ident(TermName(fullyQualified))
        case None => Ident(TermName(typeName))
      }
    }
  }

  def JsReaderObjectRepGen(termPackageMap: Map[String, String]) = new JsReaderGen {
    private def generateFieldErrors(className: String, fields: List[ModelParameter], errorType: Ident): List[Tree] = {
      fields.flatMap(f => {
        Seq(
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.value.capitalize}InvalidError"), Template(List(errorType), noSelfType, List())), // TODO: Should grab a JsReader[A] and be a case class
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.value.capitalize}MissingError"), Template(List(errorType), noSelfType, List()))
        )
      })
    }

    private def generateDefaultValue(parameter: ModelParameter, defaultValues: ModelDefaultParameterValues): Tree = {
      defaultValues.map.get(parameter.term.value) match {
        case Some(value) => Apply(Ident(TermName("Some")), List(Literal(Constant(value))))
        case None => Ident(TermName("None"))
      }
    }

    private def generateFieldExtractors(className: String, fields: List[ModelParameter], defaultValues: ModelDefaultParameterValues, errorType: Ident): List[Tree] = {
      fields.map(f => {
        val modelType = getClassType(f.parameterType, termPackageMap)

        ValDef(
          Modifiers(),
          TermName(s"${f.term.value}Extractor"),
          TypeTree(),
          Apply(
            TypeApply(Ident(TermName("JsonObjectValueExtractor")), List(modelType, errorType)),
            List(
              AssignOrNamedArg(Ident(TermName("key")), Literal(Constant(f.term.value))),
              AssignOrNamedArg(Ident(TermName("missing")), Ident(TermName(s"$className${f.term.value.capitalize}MissingError"))),
              AssignOrNamedArg(Ident(TermName("invalid")), Function(
                List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
                Ident(TermName(s"$className${f.term.value.capitalize}InvalidError"))
              )),
              AssignOrNamedArg(Ident(TermName("default")), generateDefaultValue(f, defaultValues))
            )
          )
        )
      })
    }

    private def generateModelMap(fields: List[ModelParameter], inner: Tree): Tree = fields match {
      case f :: fs => {
        val method = if (fs == Nil) "map" else "flatMap"
        Apply(
          Select(
            Apply(Ident(TermName(s"${f.term.value}Extractor")), List(Ident(TermName("map")))),
            TermName(method)
          ),
          List(
            Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName(f.term.value), TypeTree(), EmptyTree)),
              generateModelMap(fs, inner)
            )
          )
        )
      }
      case Nil => inner
    }

    private def generateModelAssignments(fields: List[ModelParameter]): List[AssignOrNamedArg] = {
      fields.map(f => AssignOrNamedArg(Ident(TermName(f.term.value)), Ident(TermName(f.term.value))))
    }

    def generate(model: Model): Tree = {
      val modelName = model.name.value

      val modelClass = Ident(TermName(model.fullyQualifiedName))
      val modelJsReaderError = Ident(TermName(s"${modelName}JsReaderError"))

      ModuleDef(
        Modifiers(),
        TermName(s"${modelName}JsReader"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsReader")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          combine(
            TypeDef(Modifiers(Flag.OVERRIDE), TypeName("JsReaderFailure"), List(), modelJsReaderError),
            ClassDef(Modifiers(Flag.TRAIT | Flag.SEALED), TypeName(s"${modelName}JsReaderError"), List(), Template(List(Ident(TermName(("AnyRef")))), noSelfType, List())),
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}NotJsonObject"), Template(List(modelJsReaderError), noSelfType, List())),

            generateFieldErrors(modelName, model.parameters, modelJsReaderError),
            generateFieldExtractors(modelName, model.parameters, model.defaultValues, modelJsReaderError),

            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("read"),
              List(),
              List(
                List(ValDef(Modifiers(Flag.PARAM), TermName("value"), Ident(TypeName("JsValue")), EmptyTree))
              ),
              TypeApply(Ident(TermName("Validation")), List(modelJsReaderError, modelClass)),
              Match(
                Ident(TermName("value")),
                List(
                  CaseDef(
                    Apply(Ident(TermName("JsObject")), List(Bind(TermName("map"), Ident(termNames.WILDCARD)))),
                    EmptyTree,
                    generateModelMap(
                      model.parameters,
                      Apply(modelClass, generateModelAssignments(model.parameters))
                    )
                  ),
                  CaseDef(Ident(termNames.WILDCARD), EmptyTree, Apply(Ident(TermName("Failure")), Ident(TermName(s"${modelName}NotJsonObject"))))
                )
              )
            )
          )
        )
      )
    }
  }

  def JsReaderParameterRepGen(termPackageMap: Map[String, String]) = new JsReaderGen {
    def generate(model: Model): Tree = {
      val modelName = model.name.value

      val modelClass = Ident(TermName(model.fullyQualifiedName))
      val modelJsReaderError = Ident(TermName(s"${modelName}JsReaderError"))

      val parameter = model.parameters.head

      ModuleDef(
        Modifiers(),
        TermName(s"${model.name.value}JsReader"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsReader")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          List(
            TypeDef(Modifiers(Flag.OVERRIDE), TypeName("JsReaderFailure"), List(), modelJsReaderError),
            ClassDef(Modifiers(Flag.TRAIT | Flag.SEALED), TypeName(s"${modelName}JsReaderError"), List(), Template(List(Ident(TermName(("AnyRef")))), noSelfType, List())),
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}InvalidJsonType"), Template(List(modelJsReaderError), noSelfType, List())),

            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("read"),
              List(),
              List(
                List(ValDef(Modifiers(Flag.PARAM), TermName("value"), Ident(TypeName("JsValue")), EmptyTree))
              ),
              TypeApply(Ident(TermName("Validation")), List(modelJsReaderError, modelClass)),
              Apply(
                Select(
                  Apply(
                    Select(
                      TypeApply(
                        Select(Ident(TermName("value")), TermName("as")),
                        List(getClassType(parameter.parameterType, termPackageMap))
                      ),
                      TermName("mapError")
                    ),
                    List(
                      Function(
                        List(ValDef(Modifiers(Flag.PARAM), TermName("_"), TypeTree(), EmptyTree)),
                        Ident(TermName(s"${modelName}InvalidJsonType"))
                      )
                    )
                  ),
                  TermName("map")
                ),
                List(
                  Function(
                    List(ValDef(Modifiers(Flag.PARAM), TermName("f"), TypeTree(), EmptyTree)),
                    Apply(modelClass, List(Ident(TermName("f"))))
                  )
                )
              )
            )
          )
        )
      )
    }
  }

}
