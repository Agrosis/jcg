package com.plasmaconduit.json.codegen

import scala.reflect.runtime.universe._

import scala.tools.reflect.ToolBox

object ModelGenerator {

  private def checkIfParentExists(parents: List[Tree], parent: String): Boolean = parents match {
    case x :: xs => x match {
      case Ident(TypeName(name)) if name == parent => true
      case _ => checkIfParentExists(xs, parent)
    }
    case Nil => false
  }

  private def getTypeParameters(typeParameters: List[Tree]): List[ModelFieldType] = {
    typeParameters.map{
      case Ident(TypeName(name)) => ModelFieldType(name, List())
      case AppliedTypeTree(Ident(TypeName(name)), typeParameters) => ModelFieldType(name, getTypeParameters(typeParameters))
    }
  }

  private def getFields(body: List[Tree]): List[ModelField] = body match {
    case ValDef(modifiers, TermName(termName), Ident(TypeName(typeName)), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelField(ModelFieldTerm(termName), ModelFieldType(typeName, List())) :: getFields(xs)
    }
    case ValDef(modifiers, TermName(termName), AppliedTypeTree(Ident(TypeName(typeName)), typeParameters), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelField(ModelFieldTerm(termName), ModelFieldType(typeName, getTypeParameters(typeParameters))) :: getFields(xs)
    }
    case x :: xs => getFields(xs)
    case Nil => Nil
  }

  private def traverseForModels(ast: Tree): List[Model] = {
    ast match {
      case Block(body, expr) => {
        body.flatMap(childAst => traverseForModels(childAst))
      }
      case ModuleDef(modifiers, termName, Template(parents, self, body)) => {
        body.flatMap(childAst => traverseForModels(childAst))
      }
      case ClassDef(modifiers, TypeName(name), typeDef, Template(parents, self, body)) => {
        List(Model(ModelName(name), getFields(body), checkIfParentExists(parents, "GenReader"), checkIfParentExists(parents, "GenWriter")))
      }
      case _ => List()
    }
  }

  def generateModelsFor(code: String): List[Model] = {
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
    val ast = tb.parse(code)

    println(showRaw(ast))

    traverseForModels(ast)
  }

}
