package com.plasmaconduit.json.codegen

import java.io.{File, PrintWriter}

import com.plasmaconduit.json.codegen.gen.{JsReaderGen, JsWriterGen}

import com.plasmaconduit.json.codegen.model._
import com.plasmaconduit.json.codegen.utils.PackageTraverser
import com.plasmaconduit.json.codegen.utils.Path._

import scala.reflect.runtime.universe._

object JsGenerator {

  def generateJsWriterImplicit(model: Model): Tree = {
    ValDef(Modifiers(Flag.IMPLICIT | Flag.LAZY), TermName(s"${model.name}JsWriterImplicit"), TypeTree(), Ident(TermName(s"${model.name}JsWriter")))
  }


  def generateJsWriter(model: Model, traitModels: List[TraitModel], classModels: List[ClassModel], termPackageMap: Map[String, String]): Tree = {
    model match {
      case cm: ClassModel => {
        val isChild = cm.parents.foldLeft(false)((b, a) => b || traitModels.exists(_.name == a))
        cm.genWriterRep match {
          case Some(ModelObjectRep(ignore)) => JsWriterGen.JsWriterObjectRepGen(ignore, isChild).generate(cm)
          case Some(ModelParameterRep) => JsWriterGen.JsWriterParameterRepGen().generate(cm)
          case None => EmptyTree
        }
      }
      case tm: TraitModel => {
        val children = classModels.filter(_.hasParent(tm.name))

        JsWriterGen.JsWriterTraitGen(children, termPackageMap).generate(tm)
      }
    }
  }

  def generateJsReaderImplicit(model: Model): Tree = {
    ValDef(Modifiers(Flag.IMPLICIT | Flag.LAZY), TermName(s"${model.name}JsReaderImplicit"), TypeTree(), Ident(TermName(s"${model.name}JsReader")))
  }


  def generateJsReader(model: Model, traitModels: List[TraitModel], classModels: List[ClassModel], termPackageMap: Map[String, String]): Tree = {
    model match {
      case cm: ClassModel => {
        cm.genReaderRep match {
          case Some(ModelObjectRep(ignore)) => JsReaderGen.JsReaderObjectRepGen(termPackageMap).generate(cm)
          case Some(ModelParameterRep) => JsReaderGen.JsReaderParameterRepGen(termPackageMap).generate(cm)
          case None => EmptyTree
        }
      }
      case tm: TraitModel => {
        val children = classModels.filter(_.hasParent(tm.name))

        JsReaderGen.JsReaderTraitGen(children, termPackageMap).generate(tm)
      }
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
      val models = ModelCollector.generateModelsFor(code)

      println(s"Visiting ${file.getAbsolutePath}...")
      println(s"Found ${models.length} models...")

      models
    })

    val classModels = models.flatMap {
      case cm: ClassModel => List(cm)
      case _ => List()
    }

    val traitModels = models.flatMap {
      case tm: TraitModel => List(tm)
      case _ => List()
    }

    val termPackageMap = models.map(m => (m.name, m.fullyQualifiedName)).toMap
    val writers = models.filter(_.hasParent("GenWriter"))
    val readers = models.filter(_.hasParent("GenReader"))

    val genJsWriters = PackageDef(
      Ident(TermName(s"$outputPackage.writers")),
      List(
        Import(Select(Select(Ident(TermName("com")), TermName("plasmaconduit")), TermName("json")), List(ImportSelector(termNames.WILDCARD, 56, null, -1))),
        ModuleDef(
          Modifiers(),
          TermName("GenJsWriters"),
          Template(
            List(Ident(TermName("AnyRef"))),
            noSelfType,
            writers.map(generateJsWriterImplicit) ++ writers.map(generateJsWriter(_, traitModels, classModels, termPackageMap))
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
            List(Ident(TermName("AnyRef"))),
            noSelfType,
            readers.map(generateJsReaderImplicit) ++ readers.map(generateJsReader(_, traitModels, classModels, termPackageMap))
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
