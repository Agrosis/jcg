package com.plasmaconduit.json.codegen.gen

import com.plasmaconduit.json.codegen.model._

import scala.reflect.runtime.universe._

sealed trait JsReaderGen[A <: Model] {
  def generate(model: A): Tree
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
      TypeApply(Ident(TermName("Map")), List(getClassType(innerTypeParameters.head, termPackageMap), getClassType(innerTypeParameters.drop(1).head, termPackageMap)))
    }
    case ClassModelParameterType("List", innerTypeParameters) => {
      TypeApply(Ident(TermName("List")), List(getClassType(innerTypeParameters.head, termPackageMap)))
    }
    case ClassModelParameterType("Option", innerTypeParameters) => {
      TypeApply(Ident(TermName("Option")), List(getClassType(innerTypeParameters.head, termPackageMap)))
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

  def JsReaderObjectRepGen(termPackageMap: Map[String, String]) = new JsReaderGen[ClassModel] {
    private def generateFieldErrors(className: String, fields: List[ClassModelParameter], errorTrait: Ident): List[Tree] = {
      fields.flatMap(f => {
        Seq(
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.capitalize}InvalidError"), Template(List(errorTrait), noSelfType, List())), // TODO: Should grab a JsReader[A] and be a case class
          ModuleDef(Modifiers(Flag.CASE), TermName(s"$className${f.term.capitalize}MissingError"), Template(List(errorTrait), noSelfType, List()))
        )
      })
    }

    private def generateDefaultValue(parameter: ClassModelParameter, defaultValues: Map[String, Tree]): Tree = {
      defaultValues.get(parameter.term) match {
        case Some(tree) => Apply(Ident(TermName("Some")), List(tree))
        case None => Ident(TermName("None"))
      }
    }

    private def generateFieldExtractors(className: String, fields: List[ClassModelParameter], defaultValues: Map[String, Tree], customReaders: Map[String, Tree], errorType: Ident): List[Tree] = {
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

    private def generateModelMap(fields: List[ClassModelParameter], inner: Tree): Tree = {
      def generateModelMapWithMultipleFields(fields: List[ClassModelParameter], inner: Tree): Tree = {
        fields match {
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
                  Block(List(), generateModelMapWithMultipleFields(fs, inner))
                )
              )
            )
          }
          case Nil => inner
        }
      }

      if (fields.isEmpty) {
        Apply(Ident(TermName("Success")), List(inner))
      } else {
        generateModelMapWithMultipleFields(fields, inner)
      }
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

  def JsReaderParameterRepGen(termPackageMap: Map[String, String]) = new JsReaderGen[ClassModel] {
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

  def JsReaderTraitGen(children: List[ClassModel], termPackageMap: Map[String, String]) = new JsReaderGen[TraitModel] {
    private def generateChildErrors(modelName: String, children: List[ClassModel], errorTrait: Ident): List[Tree] = {
      children.map(c => ModuleDef(Modifiers(Flag.CASE), TermName(s"$modelName${c.name}Error"), Template(List(errorTrait), noSelfType, List())))
    }

    private def generateCases(modelName: String, children: List[ClassModel]): List[CaseDef] = {
      children.map(c => {
        CaseDef(
          Literal(Constant(c.typeName.getOrElse(c.name))),
          EmptyTree,
          Apply(
            Select(
              Apply(
                Select(Ident(TermName(s"${c.name}JsReader")), TermName("read")),
                List(Ident(TermName("obj")))
              ),
              TermName("mapError")
            ),
            List(
              Function(
                List(ValDef(Modifiers(Flag.PARAM), TermName("e"), TypeTree(), EmptyTree)),
                Ident(TermName(s"$modelName${c.name}Error"))
              )
            )
          )
        )
      }) :+ CaseDef(Ident(termNames.WILDCARD), EmptyTree, Apply(Ident(TermName("Failure")), List(Ident(TermName(s"${modelName}TypeInvalidError")))))
    }

    override def generate(model: TraitModel): Tree = {
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
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}TypeInvalidJsonTypeError"), Template(List(modelJsReaderError), noSelfType, List())),
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}TypeInvalidError"), Template(List(modelJsReaderError), noSelfType, List())),
            ModuleDef(Modifiers(Flag.CASE), TermName(s"${modelName}TypeMissingError"), Template(List(modelJsReaderError), noSelfType, List())),

            generateChildErrors(modelName, children, modelJsReaderError),

            ValDef(
              Modifiers(),
              TermName("typeExtractor"),
              TypeTree(),
              Apply(
                TypeApply(Ident(TermName("JsonObjectValueExtractor")), List(Ident(TermName("String")), modelJsReaderError)),
                List(
                  AssignOrNamedArg(Ident(TermName("key")), Literal(Constant(model.typeField))),
                  AssignOrNamedArg(Ident(TermName("missing")), Ident(TermName(s"${modelName}TypeMissingError"))),
                  AssignOrNamedArg(Ident(TermName("invalid")), Function(
                    List(ValDef(Modifiers(Flag.PARAM), TermName("e"), TypeTree(), EmptyTree)),
                    Ident(TermName(s"${modelName}TypeInvalidJsonTypeError"))
                  )),
                  AssignOrNamedArg(Ident(TermName("default")), Ident(TermName("None")))
                )
              )
            ),

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
                      Apply(
                        Select(
                          Apply(Ident(TermName("typeExtractor")), List(Ident(TermName("map")))),
                          TermName("flatMap")
                        ),
                        List(
                          Function(
                            List(ValDef(Modifiers(Flag.PARAM), TermName("childType"), TypeTree(), EmptyTree)),
                            Apply(
                              Ident(TermName("readFromType")),
                              List(Ident(TermName("map")), Ident(TermName("childType")))
                            )
                          )
                        )
                      )
                    ),
                    CaseDef(Ident(termNames.WILDCARD), EmptyTree, Apply(Ident(TermName("Failure")), Ident(TermName(s"${modelName}NotJsonObject"))))
                  )
                )
              )
            ),

            DefDef(
              Modifiers(),
              TermName("readFromType"),
              List(),
              List(
                List(
                  ValDef(Modifiers(Flag.PARAM), TermName("map"), TypeApply(Ident(TermName("Map")), List(Ident(TermName("String")), Ident(TermName("JsValue")))), EmptyTree),
                  ValDef(Modifiers(Flag.PARAM), TermName("childType"), Ident(TypeName("String")), EmptyTree)
                )
              ),
              TypeApply(Ident(TermName("Validation")), List(modelJsReaderError, modelClass)),
              Block(
                List(
                  ValDef(Modifiers(), TermName("obj"), TypeTree(), Apply(Ident(TermName("JsObject")), Ident(TermName("map"))))
                ),
                Match(
                  Ident(TermName("childType")),
                  generateCases(modelName, children)
                )
              )
            )
          )
        )
      )
    }
  }

}
