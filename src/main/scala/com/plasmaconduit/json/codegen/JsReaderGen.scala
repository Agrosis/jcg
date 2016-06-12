package com.plasmaconduit.json.codegen

import scala.reflect.runtime.{universe => ru, currentMirror => cm}

import treehugger.forest._
import treehuggerDSL._
import definitions._

object JsReaderGen {

  private def getClassType(typeSignature: ru.Type): treehugger.forest.Type = typeSignature match {
    case ts if typeSignature =:= ru.typeOf[Boolean] => BooleanClass
    case ts if typeSignature =:= ru.typeOf[Long] => LongClass
    case ts if typeSignature =:= ru.typeOf[String] => StringClass
    case ts if typeSignature =:= ru.typeOf[Float] => FloatClass
    case ts if typeSignature <:< ru.typeOf[List[_]] => {
      val innerType = getClassType(typeSignature.asInstanceOf[ru.TypeRefApi].args.head)
      ListClass.TYPE_OF(innerType)
    }
    case ts if typeSignature <:< ru.typeOf[Map[String, _]] => {
      val innerType = getClassType(typeSignature.asInstanceOf[ru.TypeRefApi].args.drop(1).head)
      MapClass.TYPE_OF(StringClass, innerType)
    }
    case _ => RootClass.newClass(typeSignature.typeSymbol.name.toString())
  }

  private def generateFieldErrors(className: String, fields: Iterable[ru.Symbol], errorType: treehugger.forest.Symbol): Seq[treehugger.forest.Tree] = {
    fields.flatMap(s => {
      val name = s.name.toString().trim()

      Seq(
        CASEOBJECTDEF(s"$className${name.capitalize}InvalidError").withParents(errorType),
        CASEOBJECTDEF(s"$className${name.capitalize}MissingError").withParents(errorType)
      )
    })
  }

  private def generateFieldExtractors(className: String, fields: Iterable[ru.Symbol], errorType: treehugger.forest.Symbol): Seq[treehugger.forest.Tree] = {
    fields.map(s => {
      val name = s.name.toString().trim()
      val modelType = getClassType(s.typeSignature)

      VAL(s"${name}Extractor") := symbols.JsonObjectValueExtractorFunction(modelType, errorType).APPLY(
        REF("key") := LIT(name),
        REF("missing") := REF(s"$className${name.capitalize}MissingError"),
        REF("invalid") := LAMBDA(PARAM(WILDCARD)) ==> REF(s"$className${name.capitalize}InvalidError")
      )
    })
  }

  private def generateForComprehensionAssignments(fields: Iterable[ru.Symbol]): Seq[treehugger.forest.ForValFrom] = {
    fields.map(s => {
      val name = s.name.toString().trim()
      VALFROM(name) := REF(name + "Extractor").APPLY(REF("map"))
    }).toSeq
  }

  private def generateModelAssignments(fields: Iterable[ru.Symbol]): Seq[treehugger.forest.Tree] = {
    fields.map(s => {
      val name = s.name.toString().trim()
      REF(name) := REF(name)
    }).toSeq
  }

  def generateJsReaderFor(c: Class[_]): treehugger.forest.Tree = {
    val className = c.getSimpleName()
    val fields = cm.classSymbol(c).toType.members.filter(!_.isMethod)

    val modelClass = RootClass.newClass(className)
    val modelJsReaderError = RootClass.newAbstractType(s"${className}JsReaderError")

    OBJECTDEF(s"${className}JsReader").withFlags(Flags.IMPLICIT).withParents(symbols.JsReaderType.APPLYTYPE(className)) := BLOCK(
      combine(
        TYPEVAR(symbols.JsReaderFailureAliasType)/*.withFlags(Flags.OVERRIDE)*/ := REF(modelJsReaderError),
        TRAITDEF(modelJsReaderError).withFlags(Flags.SEALED),
        CASEOBJECTDEF(s"${className}NotJsonObject").withParents(modelJsReaderError),

        generateFieldErrors(className, fields, modelJsReaderError),
        generateFieldExtractors(className, fields, modelJsReaderError),

        DEF("read", symbols.Validation(modelJsReaderError, modelClass)).withParams(PARAM("value", symbols.JsValueType)).withFlags(Flags.OVERRIDE) := BLOCK(
          REF("value") MATCH(
            CASE (symbols.JsObjectClass.APPLY(REF("map"))) ==> BLOCK(
              FOR(
                generateForComprehensionAssignments(fields)
              ) YIELD REF(modelClass).APPLY(generateModelAssignments(fields))
            ),
            CASE (WILDCARD) ==> symbols.Failure.APPLY(REF(s"${className}NotJsonObject"))
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

  def main(args: Array[String]): Unit = {
    println(
      treeToString(
        generateJsReaderFor(Class.forName("org.company.app.models.User"))
      )
    )
  }

}
