package com.plasmaconduit.json.codegen

import java.io.{File, PrintWriter}

import com.plasmaconduit.json.codegen.gen.{JsReaderGen, JsWriterGen}

import com.plasmaconduit.json.codegen.model.{ModelParameterRep, ModelObjectRep, ModelGenerator, Model}
import com.plasmaconduit.json.codegen.utils.PackageTraverser
import com.plasmaconduit.json.codegen.utils.Path._

import scala.reflect.runtime.universe._

object JsGenerator {

  def generateJsWriterImplicit(model: Model): Tree = {
    ValDef(Modifiers(Flag.IMPLICIT | Flag.LAZY), TermName(s"${model.name.value}JsWriterImplicit"), TypeTree(), Ident(TermName(s"${model.name.value}JsWriter")))
  }


  def generateJsWriter(model: Model): Tree = {
    model.genWriterRep match {
      case Some(ModelObjectRep(ignore)) => JsWriterGen.JsWriterObjectRepGen(ignore).generate(model)
      case Some(ModelParameterRep) => JsWriterGen.JsWriterParameterRepGen().generate(model)
      case None => EmptyTree
    }
  }

  def generateJsReaderImplicit(model: Model): Tree = {
    ValDef(Modifiers(Flag.IMPLICIT | Flag.LAZY), TermName(s"${model.name.value}JsReaderImplicit"), TypeTree(), Ident(TermName(s"${model.name.value}JsReader")))
  }


  def generateJsReader(model: Model, termPackageMap: Map[String, String]): Tree = {
    model.genReaderRep match {
      case Some(ModelObjectRep(ignore)) => JsReaderGen.JsReaderObjectRepGen(termPackageMap).generate(model)
      case Some(ModelParameterRep) => JsReaderGen.JsReaderParameterRepGen(termPackageMap).generate(model)
      case None => EmptyTree
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      println("Arguments: [ROOT_DIRECTORY] [SOURCES_DIRECTORY] [MODELS_PACKAGE] [OUTPUT_PACKAGE]")
      System.exit(0)
    }

    val rootDir = args(0) // .
    val sourceDir = args(1) // src/main/scala/
    val searchPackage = args(2) // org.company
    val outputPackage = args(3) // json
    val outputPackagePath = packageToPath(outputPackage)

    val models = PackageTraverser.getAllClassesInPackage(rootDir, sourceDir, searchPackage).flatMap(file => {
      val code = scala.io.Source.fromFile(file.getAbsolutePath).mkString
      val models = ModelGenerator.generateModelsFor(code)

      println(s"Visiting ${file.getAbsolutePath}...")
      println(s"Found ${models.length} models...")

      models
    })

    val termPackageMap = models.map(m => (m.name.value, m.fullyQualifiedName)).toMap
    val writers = models.filter(_.genWriterRep.isDefined)
    val readers = models.filter(_.genReaderRep.isDefined)

    val genJsWriters = PackageDef(
      Ident(TermName(s"$outputPackage.writers")),
      List(
        Import(Select(Select(Ident(TermName("com")), TermName("plasmaconduit")), TermName("json")), List(ImportSelector(termNames.WILDCARD, 56, null, -1))),
        ModuleDef(
          Modifiers(),
          TermName("GenJsWriters"),
          Template(
            List(),
            noSelfType,
            writers.map(generateJsWriterImplicit) ++ writers.map(generateJsWriter)
          )
        )
      )
    ).toString()

    val genJsReaders = PackageDef(
      Ident(TermName(s"$outputPackage.readers")),
      List(
        Import(Select(Select(Ident(TermName("com")), TermName("plasmaconduit")), TermName("json")), List(ImportSelector(termNames.WILDCARD, 56, null, -1))),
        Import(Select(Select(Ident(TermName("com")), TermName("plasmaconduit")), TermName("validation")), List(ImportSelector(termNames.WILDCARD, 56, null, -1))),
        ModuleDef(
          Modifiers(),
          TermName("GenJsReaders"),
          Template(
            List(),
            noSelfType,
            readers.map(generateJsReaderImplicit) ++ readers.map(generateJsReader(_, termPackageMap))
          )
        )
      )
    ).toString()

    val writersDir = new File(rootDir / sourceDir / outputPackagePath / "writers")
    if (!writersDir.exists()) writersDir.mkdirs()

    val jsWritersWriter = new PrintWriter(new File(rootDir / sourceDir / outputPackagePath / "writers" + "GenJsWriters.scala"))
    jsWritersWriter.write(genJsWriters)
    jsWritersWriter.close()

    val readersDir = new File(rootDir / sourceDir / outputPackagePath / "readers")
    if (!readersDir.exists()) readersDir.mkdirs()

    val jsReadersWriter = new PrintWriter(new File(rootDir / sourceDir / outputPackagePath / "readers" + "GenJsReaders.scala"))
    jsReadersWriter.write(genJsReaders)
    jsReadersWriter.close()
  }

}
