package com.plasmaconduit.json.codegen

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

import treehugger.forest._
import treehuggerDSL._

object JsWriterGen {

  private def getJsClassSymbol(typeSignature: ru.Type): Option[treehugger.forest.ClassSymbol] = typeSignature match {
    case ts if typeSignature =:= ru.typeOf[Boolean] => Some(symbols.JsBooleanClass)
    case ts if typeSignature =:= ru.typeOf[Long] => Some(symbols.JsLongClass)
    case ts if typeSignature =:= ru.typeOf[String] => Some(symbols.JsStringClass)
    case ts if typeSignature =:= ru.typeOf[Float] => Some(symbols.JsFloatClass)
    case ts if typeSignature <:< ru.typeOf[List[_]] => Some(symbols.JsArrayClass)
    case ts if typeSignature <:< ru.typeOf[Map[String, _]] => Some(symbols.JsObjectClass)
    case _ => None
  }

  private def applyMapValuesToJsObjectTree(tree: treehugger.forest.Tree, typeParameter: ru.Type): treehugger.forest.Tree = {
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

  private def applyMapToJsArrayTree(tree: treehugger.forest.Tree, typeParameter: ru.Type): treehugger.forest.Tree = {
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

  private def getJsValueTypeTree(name: String, typeSignature: ru.Type): treehugger.forest.Tree = {
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
    val className = c.getSimpleName()

    OBJECTDEF(s"${className}JsWriter").withFlags(Flags.IMPLICIT).withParents(symbols.JsWriterType.APPLYTYPE(className)) := BLOCK(
      DEF("write", symbols.JsValueType).withParams(PARAM("m", className)).withFlags(Flags.OVERRIDE) := BLOCK(
        LIT(symbols.JsObjectClass).APPLY(
          cm.classSymbol(Class.forName("org.company.app.models.User")).toType.members.filter(!_.isMethod).map(s => {
            val name = s.name.toString().trim()

            TUPLE(LIT(name), getJsValueTypeTree(name, s.typeSignature))
          }).toSeq: _*
        )
      )
    )
  }

}
