package com.plasmaconduit.json.codegen

import java.io.{File, PrintWriter}

import com.plasmaconduit.json.codegen.generators.{JsWriterGen, JsReaderGen}
import treehugger.forest._
import treehuggerDSL._

object JsonCodeGenerator {

  def generateJsWriterImplicit(model: Model): treehugger.forest.Tree = {
    VAL(s"${model.name.value}JsWriterImplicit").withFlags(Flags.IMPLICIT) := REF(s"${model.name.value}JsWriter")
  }

  def generateJsWriter(model: Model): treehugger.forest.Tree = {
    JsWriterGen.generateJsWriterFor(model)
  }

  def generateJsReaderImplicit(model: Model): treehugger.forest.Tree = {
    VAL(s"${model.name.value}JsReaderImplicit").withFlags(Flags.IMPLICIT) := REF(s"${model.name.value}JsReader")
  }

  def generateJsReader(model: Model, termPackageMap: Map[String, String]): treehugger.forest.Tree = {
    new JsReaderGen(termPackageMap).generateJsReaderFor(model)
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

    val termPackageMap = models.map(m => (m.name.value, m.fullyQualifiedName)).toMap
    val writers = models.filter(_.genWriter)
    val readers = models.filter(_.genReader)

    val genJsWriters = treehugger.forest.treeToString(
      BLOCK(
        IMPORT("com.plasmaconduit.json._"),
        OBJECTDEF("GenJsWriters") := BLOCK(
          writers.map(generateJsWriterImplicit) ++
          writers.map(generateJsWriter)
        )
      ).inPackage("json.writers")
    )

    val genJsReaders = treehugger.forest.treeToString(
      BLOCK(
        IMPORT("com.plasmaconduit.json._"),
        IMPORT("com.plasmaconduit.validation._"),
        OBJECTDEF("GenJsReaders") := BLOCK(
          readers.map(generateJsReaderImplicit) ++
          readers.map(m => generateJsReader(m, termPackageMap))
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
