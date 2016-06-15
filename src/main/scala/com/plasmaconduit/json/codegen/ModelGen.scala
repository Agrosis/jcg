package com.plasmaconduit.json.codegen

import scala.reflect.runtime.universe._

import scala.tools.reflect.ToolBox

object ModelGen {

  @annotation.tailrec
  private def checkIfParentExists(parents: List[Tree], parent: String): Boolean = parents match {
    case x :: xs => x match {
      case Ident(TypeName(name)) if name == parent => true
      case _ => checkIfParentExists(xs, parent)
    }
    case Nil => false
  }

  private def getTypeParameters(typeParameters: List[Tree]): List[ModelParameterType] = {
    typeParameters.map{
      case Ident(TypeName(name)) => ModelParameterType(name, List())
      case AppliedTypeTree(Ident(TypeName(name)), typeParameters) => ModelParameterType(name, getTypeParameters(typeParameters))
    }
  }

  private def getFields(ast: List[Tree]): List[ModelParameter] = ast match {
    case ValDef(modifiers, TermName(termName), Ident(TypeName(typeName)), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelParameter(ModelParameterTerm(termName), ModelParameterType(typeName, List())) :: getFields(xs)
    }
    case ValDef(modifiers, TermName(termName), AppliedTypeTree(Ident(TypeName(typeName)), typeParameters), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelParameter(ModelParameterTerm(termName), ModelParameterType(typeName, getTypeParameters(typeParameters))) :: getFields(xs)
    }
    case x :: xs => getFields(xs)
    case Nil => Nil
  }

  private def getDefaultParameters(ast: List[Tree]): Map[String, Any] = {
    def find(defs: List[ValDef]): List[(String, Any)] = defs match {
      case ValDef(modifiers, TermName(termName), typeName, Literal(Constant(defaultValue))) :: xs if modifiers.hasFlag(Flag.DEFAULTPARAM) => {
        (termName, defaultValue) :: find(xs)
      }
      case x :: xs => find(xs)
      case Nil => Nil
    }

    ast match {
      case DefDef(modifiers, termNames.CONSTRUCTOR, typeDefs, valDefs :: n, typeTree, block) :: x => { // TODO: There can be multiple parameter lists for valDefs
        find(valDefs).toMap
      }
      case x :: xs => getDefaultParameters(xs)
      case Nil => Map()
    }
  }

  private def traverseForModels(ast: Tree, packageName: String): List[Model] = {
    ast match {
      case Block(body, expr) => {
        body.flatMap(childAst => traverseForModels(childAst, packageName))
      }
      case ModuleDef(modifiers, termName, Template(parents, self, body)) => {
        body.flatMap(childAst => traverseForModels(childAst, packageName))
      }
      case ClassDef(modifiers, TypeName(name), typeDef, Template(parents, self, body)) => {
        List(
          Model(
            ModelName(name),
            ModelPackage(packageName),
            getFields(body),
            ModelDefaultParameterValues(getDefaultParameters(body)),
            checkIfParentExists(parents, "GenReader"),
            checkIfParentExists(parents, "GenWriter")
          )
        )
      }
      case _ => List()
    }
  }

  def generateModelsFor(code: String): List[Model] = {
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    val packageName = "^(\\s+)?package (.*)".r.findFirstIn(code).map(_.split(" ")(1)).get // TODO: Fix

    try {
      val ast = tb.parse(code.replaceFirst("^(\\s+)?package", "//package"))
//      println(showRaw(ast))
      traverseForModels(ast, packageName)
    } catch {
      case e: Throwable => {
        println(e)
        List()
      }
    }
  }

}
