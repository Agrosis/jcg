package com.plasmaconduit.json.codegen

import treehugger.forest._
import treehuggerDSL._
import definitions._

object JsReaderGen {

  private def getClassType(fieldType: ModelFieldType): treehugger.forest.Type = fieldType match {
    case ModelFieldType("Map", innerTypeParameters) => symbols.Map.TYPE_OF(getClassType(innerTypeParameters.head),getClassType(innerTypeParameters.drop(1).head))
    case ModelFieldType("List", innerTypeParameters) => ListClass.TYPE_OF(getClassType(innerTypeParameters.head))
    case ModelFieldType("Long", _) => LongClass
    case ModelFieldType("String", _) => StringClass
    case ModelFieldType("Float", _) => FloatClass
    case ModelFieldType("Boolean", _) => BooleanClass
    case ModelFieldType(typeName, _) => RootClass.newClass(typeName)
  }

  private def generateFieldErrors(className: String, fields: List[ModelField], errorType: treehugger.forest.Symbol): Seq[treehugger.forest.Tree] = {
    fields.flatMap(f => {
      Seq(
        CASEOBJECTDEF(s"$className${f.term.value.capitalize}InvalidError").withParents(errorType), // TODO: Should grab a JsReader[A] and be a case class
        CASEOBJECTDEF(s"$className${f.term.value.capitalize}MissingError").withParents(errorType)
      )
    })
  }

  private def generateFieldExtractors(className: String, fields: List[ModelField], errorType: treehugger.forest.Symbol): List[treehugger.forest.Tree] = {
    fields.map(f => {
      val modelType = getClassType(f.fieldType)

      VAL(s"${f.term.value}Extractor") := symbols.JsonObjectValueExtractorFunction(modelType, errorType).APPLY(
        REF("key") := LIT(f.term.value),
        REF("missing") := REF(s"$className${f.term.value.capitalize}MissingError"),
        REF("invalid") := LAMBDA(PARAM(WILDCARD)) ==> REF(s"$className${f.term.value.capitalize}InvalidError")
      )
    })
  }

  private def generateForComprehensionAssignments(fields: List[ModelField]): List[treehugger.forest.ForValFrom] = {
    fields.map(f => VALFROM(f.term.value) := REF(f.term.value + "Extractor").APPLY(REF("map")))
  }

  private def generateModelAssignments(fields: List[ModelField]): List[treehugger.forest.Tree] = {
    fields.map(f => REF(f.term.value) := REF(f.term.value))
  }

  def generateJsReaderFor(model: Model): treehugger.forest.Tree = {
    val modelName = model.name.value

    val modelClass = RootClass.newClass(modelName)
    val modelJsReaderError = RootClass.newAbstractType(s"${modelName}JsReaderError")

    OBJECTDEF(s"${modelName}JsReader").withParents(symbols.JsReaderType.APPLYTYPE(modelName)) := BLOCK(
      combine(
        TYPEVAR(symbols.JsReaderFailureAliasType)/*.withFlags(Flags.OVERRIDE)*/ := REF(modelJsReaderError),
        TRAITDEF(modelJsReaderError).withFlags(Flags.SEALED),
        CASEOBJECTDEF(s"${modelName}NotJsonObject").withParents(modelJsReaderError),

        generateFieldErrors(modelName, model.fields, modelJsReaderError),
        generateFieldExtractors(modelName, model.fields, modelJsReaderError),

        DEF("read", symbols.Validation(modelJsReaderError, modelClass)).withParams(PARAM("value", symbols.JsValueType)).withFlags(Flags.OVERRIDE) := BLOCK(
          REF("value") MATCH(
            CASE (symbols.JsObjectClass.APPLY(REF("map"))) ==> BLOCK(
              FOR(
                generateForComprehensionAssignments(model.fields)
              ) YIELD REF(modelClass).APPLY(generateModelAssignments(model.fields))
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
