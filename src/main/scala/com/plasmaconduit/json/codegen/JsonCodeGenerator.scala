package com.plasmaconduit.json.codegen

import treehugger.forest._
import treehuggerDSL._

object JsonCodeGenerator {

  def processModelForWriter(model: Model): Seq[treehugger.forest.Tree] = {
    if (model.genWriter) {
      Seq(JsWriterGen.generateJsWriterFor(model))
    } else {
      Seq()
    }
  }

  def processModelForReader(model: Model): Seq[treehugger.forest.Tree] = {
    if (model.genReader) {
      Seq(JsReaderGen.generateJsReaderFor(model))
    } else {
      Seq()
    }
  }

  def main(args: Array[String]): Unit = {
    val models = PackageTraverser.getAllClassesInPackage("org.company.app").flatMap(file => {
      val code = scala.io.Source.fromFile(file.getAbsolutePath).mkString.replace("package", "//package")
      ModelGenerator.generateModelsFor(code)
    })

    println(
      treehugger.forest.treeToString(
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          OBJECTDEF("GenJsWriters") := BLOCK(
            models.flatMap(processModelForWriter)
          )
        ).inPackage("json.writers"),
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          IMPORT("com.plasmaconduit.validation._"),
          OBJECTDEF("GenJsReaders") := BLOCK(
            models.flatMap(processModelForReader)
          )
        ).inPackage("json.readers")
      )
    )
  }

}
