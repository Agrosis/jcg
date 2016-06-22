package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model.{ClassModelParameterType, ClassModelParameter, ClassModel}

import scala.reflect.runtime.universe._

sealed trait JsReaderGen {
  def generate(model: ClassModel): Tree
}

object JsReaderGen {

  private def combine(trees: List[Tree]*): List[Tree] = {
    trees.foldLeft(List[Tree]())((b, a) => b ++ a)
  }

  private implicit def treeToListTree[A <% Tree](tree: A): List[Tree] = {
    List(tree)
  }

  private def getClassType(fieldType: ClassModelParameterType, termPackageMap: Map[String, String]): Tree = fieldType match {
    case ClassModelParameterType("Map", innerTypeParameters) => {
      TypeApply(Ident(TermName("Map")), List(getClassType(innerTypeParameters.head, termPackageMap: Map[String, String]), getClassType(innerTypeParameters.drop(1).head, termPackageMap: Map[String, String])))
    }
    case ClassModelParameterType("List", innerTypeParameters) => {
      TypeApply(Ident(TermName("List")), List(getClassType(innerTypeParameters.head, termPackageMap: Map[String, String])))
    }
    case ClassModelParameterType("Long", _) => Ident(TermName("Long"))
    case ClassModelParameterType("String", _) => Ident(TermName("String"))
    case ClassModelParameterType("Float", _) => Ident(TermName("Float"))
    case ClassModelParameterType("Boolean", _) => Ident(TermName("Boolean"))
    case ClassModelParameterType(typeName, _) => {
      termPackageMap.get(typeName) match {
        case Some(fullyQualified) => Ident(TermName(fullyQualified))
        case None => Ident(TermName(typeName))
      }
    }
  }

  private def generateCustomReaders(customReaders: Map[String, Tree]): List[Tree] = {
    customReaders.toList.map {
      case (name, tree) => ValDef(Modifiers(), TermName(name), TypeTree(), tree)
    }
  }

  def JsReaderObjectRepGen(termPackageMap: Map[String, String]) = new JsReaderGen {
    private def generateFieldErrors(className: String, fields: List[ClassModelParameter], errorType: Ident): List[Tree] = {
      fields.flatMap(f => {
        Seq(
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.capitalize}InvalidError"), Template(List(errorType), noSelfType, List())), // TODO: Should grab a JsReader[A] and be a case class
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.capitalize}MissingError"), Template(List(errorType), noSelfType, List()))
        )
      })
    }

    private def generateDefaultValue(parameter: ClassModelParameter, defaultValues: Map[String, Any]): Tree = {
      defaultValues.get(parameter.term) match {
        case Some(value) => Apply(Ident(TermName("Some")), List(Literal(Constant(value))))
        case None => Ident(TermName("None"))
      }
    }

    private def generateFieldExtractors(className: String, fields: List[ClassModelParameter], defaultValues: Map[String, Any], customReaders: Map[String, Tree], errorType: Ident): List[Tree] = {
      fields.map(f => {
        val modelType = getClassType(f.parameterType, termPackageMap)
        val fieldReaderName = s"${f.term}Reader"

        val fieldExtractor = Apply(
          TypeApply(Ident(TermName("JsonObjectValueExtractor")), List(modelType, errorType)),
          List(
            AssignOrNamedArg(Ident(TermName("key")), Literal(Constant(f.term))),
            AssignOrNamedArg(Ident(TermName("missing")), Ident(TermName(s"$className${f.term.capitalize}MissingError"))),
            AssignOrNamedArg(Ident(TermName("invalid")), Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName("x"), TypeTree(), EmptyTree)),
              Ident(TermName(s"$className${f.term.capitalize}InvalidError"))
            )),
            AssignOrNamedArg(Ident(TermName("default")), generateDefaultValue(f, defaultValues))
          )
        )

        val fieldExtractorImplicit = customReaders.get(fieldReaderName) match {
          case Some(reader) => Apply(fieldExtractor, List(Ident(TermName(fieldReaderName))))
          case None => fieldExtractor
        }

        ValDef(
          Modifiers(),
          TermName(s"${f.term}Extractor"),
          TypeTree(),
          fieldExtractorImplicit
        )
      })
    }

    private def generateModelMap(fields: List[ClassModelParameter], inner: Tree): Tree = fields match {
      case f :: fs => {
        val method = if (fs == Nil) "map" else "flatMap"
        Apply(
          Select(
            Apply(Ident(TermName(s"${f.term}Extractor")), List(Ident(TermName("map")))),
            TermName(method)
          ),
          List(
            Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName(f.term), TypeTree(), EmptyTree)),
              Block(List(), generateModelMap(fs, inner))
            )
          )
        )
      }
      case Nil => inner
    }

    private def generateModelAssignments(fields: List[ClassModelParameter]): List[AssignOrNamedArg] = {
      fields.map(f => AssignOrNamedArg(Ident(TermName(f.term)), Ident(TermName(f.term))))
    }

    def generate(model: ClassModel): Tree = {
      val modelName = model.name
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
            generateCustomReaders(model.customReaders),
            generateFieldExtractors(modelName, model.parameters, model.defaultValues, model.customReaders, modelJsReaderError),

            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("read"),
              List(),
              List(
                List(ValDef(Modifiers(Flag.PARAM), TermName("value"), Ident(TypeName("JsValue")), EmptyTree))
              ),
              TypeApply(Ident(TermName("Validation")), List(modelJsReaderError, modelClass)),
              Block(
                List(),
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
      )
    }
  }

  def JsReaderParameterRepGen(termPackageMap: Map[String, String]) = new JsReaderGen {
    def generate(model: ClassModel): Tree = {
      val modelName = model.name
      val modelClass = Ident(TermName(model.fullyQualifiedName))
      val modelJsReaderError = Ident(TermName(s"${modelName}JsReaderError"))

      val parameter = model.parameters.head
      val fieldReaderName = s"${parameter.term}Reader"

      val fieldAsApply = TypeApply(Select(Ident(TermName("value")), TermName("as")), List(getClassType(parameter.parameterType, termPackageMap)))
      val fieldApplyImplicit = model.customReaders.get(fieldReaderName) match {
        case Some(reader) => Apply(fieldAsApply, List(Ident(TermName(fieldReaderName))))
        case None => fieldAsApply
      }

      ModuleDef(
        Modifiers(),
        TermName(s"${model.name}JsReader"),
        Template(
          List(AppliedTypeTree(Ident(TypeName("JsReader")), List(Ident(TypeName(model.fullyQualifiedName))))),
          noSelfType,
          combine(
            TypeDef(Modifiers(Flag.OVERRIDE), TypeName("JsReaderFailure"), List(), modelJsReaderError),
            ClassDef(Modifiers(Flag.TRAIT | Flag.SEALED), TypeName(s"${modelName}JsReaderError"), List(), Template(List(Ident(TermName(("AnyRef")))), noSelfType, List())),
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}InvalidJsonType"), Template(List(modelJsReaderError), noSelfType, List())),

            generateCustomReaders(model.customReaders),

            DefDef(
              Modifiers(Flag.OVERRIDE),
              TermName("read"),
              List(),
              List(
                List(ValDef(Modifiers(Flag.PARAM), TermName("value"), Ident(TypeName("JsValue")), EmptyTree))
              ),
              TypeApply(Ident(TermName("Validation")), List(modelJsReaderError, modelClass)),
              Block(
                List(),
                Apply(
                  Select(
                    Apply(
                      Select(
                        fieldApplyImplicit,
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
      )
    }
  }

}
