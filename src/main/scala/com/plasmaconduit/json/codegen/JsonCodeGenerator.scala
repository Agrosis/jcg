package com.plasmaconduit.json.codegen

import treehugger.forest._
import treehuggerDSL._

object JsonCodeGenerator {

  def generateJsWriterImplicit(model: Model): List[treehugger.forest.Tree] = {
    if (model.genWriter) {
      List(
        VAL(s"${model.name.value}JsWriterImplicit").withFlags(Flags.IMPLICIT) := REF(s"${model.name.value}JsWriter")
      )
    } else {
      List()
    }
  }

  def generateJsWriter(model: Model): List[treehugger.forest.Tree] = {
    if (model.genWriter) {
      List(JsWriterGen.generateJsWriterFor(model))
    } else {
      List()
    }
  }

  def generateJsReaderImplicit(model: Model): List[treehugger.forest.Tree] = {
    if (model.genWriter) {
      List(
        VAL(s"${model.name.value}JsReaderImplicit").withFlags(Flags.IMPLICIT) := REF(s"${model.name.value}JsReader")
      )
    } else {
      List()
    }
  }

  def generateJsReader(model: Model): List[treehugger.forest.Tree] = {
    if (model.genReader) {
      List(JsReaderGen.generateJsReaderFor(model))
    } else {
      List()
    }
  }

  def main(args: Array[String]): Unit = {
    val models = PackageTraverser.getAllClassesInPackage(".", "org.company.app.models").flatMap(file => {
      val code = scala.io.Source.fromFile(file.getAbsolutePath).mkString.replace("package", "//package")
      ModelGenerator.generateModelsFor(code)
    })

    println(
      treehugger.forest.treeToString(
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          OBJECTDEF("GenJsWriters") := BLOCK(
            models.flatMap(generateJsWriterImplicit) ++
            models.flatMap(generateJsWriter)
          )
        ).inPackage("json.writers")
      )
    )

    println(
      treehugger.forest.treeToString(
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          IMPORT("com.plasmaconduit.validation._"),
          OBJECTDEF("GenJsReaders") := BLOCK(
            models.flatMap(generateJsReaderImplicit) ++
            models.flatMap(generateJsReader)
          )
        ).inPackage("json.readers")
      )
    )
  }

}
