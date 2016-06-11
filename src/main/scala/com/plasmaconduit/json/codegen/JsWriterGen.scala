package com.plasmaconduit.json.codegen

import scala.reflect.runtime.{universe => ru}

import treehugger.forest._
import definitions._
import treehuggerDSL._

object JsWriterGen {

  case class Model(a: Long, b: String, c: Boolean, d: Map[String, Map[String, Float]], e: Map[String, List[Long]])

  def getJsClassSymbol(typeSignature: ru.Type): Option[treehugger.forest.ClassSymbol] = {
    if (typeSignature =:= ru.typeOf[Boolean]) {
      Some(symbols.JsBooleanClass)
    } else if (typeSignature =:= ru.typeOf[Long]) {
      Some(symbols.JsLongClass)
    } else if (typeSignature =:= ru.typeOf[String]) {
      Some(symbols.JsStringClass)
    } else if (typeSignature =:= ru.typeOf[Float]) {
      Some(symbols.JsFloatClass)
    } else if (typeSignature <:< ru.typeOf[List[_]]) {
      Some(symbols.JsArrayClass)
    } else if (typeSignature <:< ru.typeOf[Map[String, _]]) {
      Some(symbols.JsObjectClass)
    } else {
      None
    }
  }

  def applyMapValuesToJsObjectTree(tree: treehugger.forest.Tree, typeParameter: ru.Type): treehugger.forest.Tree = {
    getJsClassSymbol(typeParameter) match {
      case Some(symbols.JsObjectClass) => {
        val innerType = typeParameter.asInstanceOf[ru.TypeRefApi].args.drop(1).head
        tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(applyMapValuesToJsObjectTree(REF("x"), innerType)))
      }
      case Some(symbols.JsArrayClass) => {
        val innerType = typeParameter.asInstanceOf[ru.TypeRefApi].args.head
        tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(applyMapToJsArrayTree(REF("x"), innerType)))
      }
      case Some(innerClassSymbol) => tree.DOT("mapValues").APPLY(LAMBDA(PARAM("x")) ==> (innerClassSymbol.APPLY(REF("x"))))
      case None => tree
    }
  }

  def applyMapToJsArrayTree(tree: treehugger.forest.Tree, typeParameter: ru.Type): treehugger.forest.Tree = {
    getJsClassSymbol(typeParameter) match {
      case Some(symbols.JsObjectClass) => {
        val innerType = typeParameter.asInstanceOf[ru.TypeRefApi].args.drop(1).head
        tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsObjectClass.APPLY(applyMapValuesToJsObjectTree(REF("x"), innerType)))
      }
      case Some(symbols.JsArrayClass) => {
        val innerType = typeParameter.asInstanceOf[ru.TypeRefApi].args.head
        tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> symbols.JsArrayClass.APPLY(applyMapToJsArrayTree(REF("x"), innerType)))
      }
      case Some(innerClassSymbol) => tree.DOT("map").APPLY(LAMBDA(PARAM("x")) ==> (innerClassSymbol.APPLY(REF("x"))))
      case None => tree
    }
  }

  def getJsValueTypeTree(name: String, typeSignature: ru.Type): treehugger.forest.Tree = {
    getJsClassSymbol(typeSignature) match {
      case Some(symbols.JsObjectClass) => {
        val innerType = typeSignature.asInstanceOf[ru.TypeRefApi].args.drop(1).head
        symbols.JsObjectClass.APPLY(applyMapValuesToJsObjectTree(REF("m").DOT(name), innerType))
      }
      case Some(symbols.JsArrayClass) => {
        val innerType = typeSignature.asInstanceOf[ru.TypeRefApi].args.head
        symbols.JsArrayClass.APPLY(applyMapToJsArrayTree(REF("m").DOT(name), innerType))
      }
      case Some(classSymbol) => classSymbol.APPLY(REF("m").DOT(name))
      case None => REF("m").DOT(name)
    }
  }

  def generateJsWriterFor(c: Class[_]): treehugger.forest.Tree = {
    val className = c.getSimpleName().replace("$", "")
    
    BLOCK(
      IMPORT("com.plasmaconduit.json", "_"),
      OBJECTDEF(s"${className}JsWriter").withFlags(Flags.IMPLICIT).withParents(symbols.JsWriterType.APPLYTYPE(className)) := BLOCK(
        DEF("write", symbols.JsValueType).withParams(PARAM("m", className)).withFlags(Flags.OVERRIDE) := BLOCK(
          LIT(symbols.JsObjectClass).APPLY(
            ru.typeOf[Model].members.filter(!_.isMethod).map(s => {
              val name = s.name.toString().trim()

              TUPLE(LIT(name), getJsValueTypeTree(name, s.typeSignature))
            }).toSeq: _*
          )
        )
      )
    ).inPackage("com.plasmaconduit.app.models")
  }

  def main(args: Array[String]): Unit = {
    val tree = generateJsWriterFor(Model.getClass())

    val str = treeToString(tree)

    println(str)
  }

}
