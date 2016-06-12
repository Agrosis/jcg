package com.plasmaconduit.json.codegen

import java.io.File

import scala.language.experimental.macros

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

import treehugger.forest._
import treehuggerDSL._

object PackageTraverser {

  def main(args: Array[String]): Unit = {
    val classes = getAllClassesInPackage("org.company.app")

    println(
      treeToString(
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          OBJECTDEF("GenJsWriters") := BLOCK(
            classes.flatMap(className => processTypeForWriters(Class.forName(className))): _*
          )
        ).inPackage("json.writers"),
        BLOCK(
          IMPORT("com.plasmaconduit.json._"),
          IMPORT("com.plasmaconduit.validation._"),
          OBJECTDEF("GenJsReaders") := BLOCK(
            classes.flatMap(className => processTypeForReaders(Class.forName(className))): _*
          )
        ).inPackage("json.readers")
      )
    )
  }

  def processTypeForWriters(clazz: Class[_]): Seq[treehugger.forest.Tree] = cm.classSymbol(clazz).toType match {
    case t if t <:< ru.typeOf[GenWriter] => Seq(JsWriterGen.generateJsWriterFor(clazz))
    case _ => Seq()
  }

  def processTypeForReaders(clazz: Class[_]): Seq[treehugger.forest.Tree] = cm.classSymbol(clazz).toType match {
    case t if t <:< ru.typeOf[GenReader] => Seq(JsReaderGen.generateJsReaderFor(clazz))
    case _ => Seq()
  }

  def getAllClassesInPackage(packageName: String): Seq[String] = {
    val directory = s"./src/main/scala/${packageName.replaceAll("\\.", "/")}/"
    traverse(new File(directory), packageName)
  }

  def traverse(file: File, packageName: String): Seq[String] = {
    file.listFiles().toList.flatMap({
      case f if f.isDirectory() => traverse(f, s"$packageName.${f.getName()}")
      case f => {
        val className = f.getName().split("\\.")(0)
        Seq(s"$packageName.$className")
      }
    })
  }

}
