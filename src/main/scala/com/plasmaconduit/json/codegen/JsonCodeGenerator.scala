package com.plasmaconduit.json.codegen

import java.io.{File, PrintWriter}

import com.plasmaconduit.json.codegen.generators.{JsWriterGen, JsReaderGen}
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
    if (model.genReader) {
      List(
        VAL(s"${model.name.value}JsReaderImplicit").withFlags(Flags.IMPLICIT) := REF(s"${model.name.value}JsReader")
      )
    } else {
      List()
    }
  }

  def generateJsReader(model: Model, termPackageMap: Map[String, String]): List[treehugger.forest.Tree] = {
    if (model.genReader) {
      List(new JsReaderGen(termPackageMap).generateJsReaderFor(model))
    } else {
      List()
    }
  }

  def main(args: Array[String]): Unit = {
    val path = "."
    val models = PackageTraverser.getAllClassesInPackage(path, "org.company").flatMap(file => {
      val code = scala.io.Source.fromFile(file.getAbsolutePath).mkString
      val models = ModelGen.generateModelsFor(code)

      println(s"Visiting ${file.getAbsolutePath}...")
      println(s"Found ${models.length} models...")

      models
    })

    val termPackageMap = models.map(m => (m.name.value, m.getFullyQualifiedName)).toMap

    val genJsWriters = treehugger.forest.treeToString(
      BLOCK(
        IMPORT("com.plasmaconduit.json._"),
        OBJECTDEF("GenJsWriters") := BLOCK(
          models.flatMap(generateJsWriterImplicit) ++
          models.flatMap(generateJsWriter)
        )
      ).inPackage("json.writers")
    )

    val genJsReaders = treehugger.forest.treeToString(
      BLOCK(
        IMPORT("com.plasmaconduit.json._"),
        IMPORT("com.plasmaconduit.validation._"),
        OBJECTDEF("GenJsReaders") := BLOCK(
          models.flatMap(generateJsReaderImplicit) ++
          models.flatMap(m => generateJsReader(m, termPackageMap))
        )
      ).inPackage("json.readers")
    )

//    println(genJsWriters)
//    println(genJsReaders)

    val writersDir = new File(s"$path/src/main/scala/json/writers/")
    if (!writersDir.exists()) writersDir.mkdirs()

    val jsWritersWriter = new PrintWriter(new File(s"$path/src/main/scala/json/writers/GenJsWriters.scala"))
    jsWritersWriter.write(genJsWriters)
    jsWritersWriter.close()

    val readersDir = new File(s"$path/src/main/scala/json/readers/")
    if (!readersDir.exists()) readersDir.mkdirs()

    val jsReadersWriter = new PrintWriter(new File(s"$path/src/main/scala/json/readers/GenJsReaders.scala"))
    jsReadersWriter.write(genJsReaders)
    jsReadersWriter.close()
  }

}
