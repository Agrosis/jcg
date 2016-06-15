package com.plasmaconduit.json.codegen

import treehugger.forest._
import treehuggerDSL._
import definitions._

class JsReaderGen(termPackageMap: Map[String, String]) {

  private def getClassType(fieldType: ModelParameterType): treehugger.forest.Type = fieldType match {
    case ModelParameterType("Map", innerTypeParameters) => symbols.Map.TYPE_OF(getClassType(innerTypeParameters.head), getClassType(innerTypeParameters.drop(1).head))
    case ModelParameterType("List", innerTypeParameters) => ListClass.TYPE_OF(getClassType(innerTypeParameters.head))
    case ModelParameterType("Long", _) => LongClass
    case ModelParameterType("String", _) => StringClass
    case ModelParameterType("Float", _) => FloatClass
    case ModelParameterType("Boolean", _) => BooleanClass
    case ModelParameterType(typeName, _) => {
      termPackageMap.get(typeName) match {
        case Some(fullyQualified) => RootClass.newClass(fullyQualified)
        case None => RootClass.newClass(typeName)
      }
    }
  }

  private def generateFieldErrors(className: String, fields: List[ModelParameter], errorType: treehugger.forest.Symbol): Seq[treehugger.forest.Tree] = {
    fields.flatMap(f => {
      Seq(
        CASEOBJECTDEF(s"$className${f.term.value.capitalize}InvalidError").withParents(errorType), // TODO: Should grab a JsReader[A] and be a case class
        CASEOBJECTDEF(s"$className${f.term.value.capitalize}MissingError").withParents(errorType)
      )
    })
  }

  private def generateDefaultValue(parameter: ModelParameter, defaultValues: ModelDefaultParameterValues): treehugger.forest.Tree = {
    defaultValues.map.get(parameter.term.value) match {
      case Some(value) => SomeClass.APPLY(LIT(value))
      case None => NONE
    }
  }

  private def generateFieldExtractors(className: String, fields: List[ModelParameter], defaultValues: ModelDefaultParameterValues, errorType: treehugger.forest.Symbol): List[treehugger.forest.Tree] = {
    fields.map(f => {
      val modelType: treehugger.forest.Type = getClassType(f.parameterType)

      VAL(s"${f.term.value}Extractor") := symbols.JsonObjectValueExtractorFunction(modelType, errorType).APPLY(
        REF("key") := LIT(f.term.value),
        REF("missing") := REF(s"$className${f.term.value.capitalize}MissingError"),
        REF("invalid") := LAMBDA(PARAM(WILDCARD)) ==> REF(s"$className${f.term.value.capitalize}InvalidError"),
        REF("default") := generateDefaultValue(f, defaultValues)
      )
    })
  }

  private def generateForComprehensionAssignments(fields: List[ModelParameter]): List[treehugger.forest.ForValFrom] = {
    fields.map(f => VALFROM(f.term.value) := REF(f.term.value + "Extractor").APPLY(REF("map")))
  }

  private def generateModelAssignments(fields: List[ModelParameter]): List[treehugger.forest.Tree] = {
    fields.map(f => REF(f.term.value) := REF(f.term.value))
  }

  def generateJsReaderFor(model: Model): treehugger.forest.Tree = {
    val modelName = model.name.value

    val modelClass = RootClass.newClass(model.getFullyQualifiedName)
    val modelJsReaderError = RootClass.newAbstractType(s"${modelName}JsReaderError")

    OBJECTDEF(s"${modelName}JsReader").withParents(symbols.JsReaderType.APPLYTYPE(model.getFullyQualifiedName)) := BLOCK(
      combine(
        TYPEVAR(symbols.JsReaderFailureAliasType)/*.withFlags(Flags.OVERRIDE)*/ := REF(modelJsReaderError),
        TRAITDEF(modelJsReaderError).withFlags(Flags.SEALED),
        CASEOBJECTDEF(s"${modelName}NotJsonObject").withParents(modelJsReaderError),

        generateFieldErrors(modelName, model.parameters, modelJsReaderError),
        generateFieldExtractors(modelName, model.parameters, model.defaultValues, modelJsReaderError),

        DEF("read", symbols.Validation(modelJsReaderError, modelClass)).withParams(PARAM("value", symbols.JsValueType)).withFlags(Flags.OVERRIDE) := BLOCK(
          REF("value") MATCH(
            CASE (symbols.JsObjectClass.APPLY(REF("map"))) ==> BLOCK(
              FOR(
                generateForComprehensionAssignments(model.parameters)
              ) YIELD REF(modelClass).APPLY(generateModelAssignments(model.parameters))
            ),
            CASE (WILDCARD) ==> symbols.Failure.APPLY(REF(s"${modelName}NotJsonObject"))
          )
        )
      )
    )
  }

  private def combine(trees: Seq[treehugger.forest.Tree]*): Seq[treehugger.forest.Tree] = {
    trees.foldLeft(Seq[treehugger.forest.Tree]())((b, a) => b ++ a)
  }

  private implicit def treeToSeqTree[A <% treehugger.forest.Tree](tree: A): Seq[treehugger.forest.Tree] = {
    Seq(tree)
  }

}
