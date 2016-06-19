package com.plasmaconduit.json.codegen.model

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object ModelGenerator {

  @annotation.tailrec
  private def parentExists(parents: List[Tree], parent: String): Boolean = parents match {
    case x :: xs => x match {
      case Ident(TypeName(name)) if name == parent => true
      case _ => parentExists(xs, parent)
    }
    case Nil => false
  }

  private def getTypeParameters(typeParameters: List[Tree]): List[ModelParameterType] = {
    typeParameters.map{
      case Ident(TypeName(name)) => ModelParameterType(name, List())
      case AppliedTypeTree(Ident(TypeName(name)), typeParameters) => ModelParameterType(name, getTypeParameters(typeParameters))
    }
  }

  private def getParameters(ast: List[Tree]): List[ModelParameter] = ast match {
    case ValDef(modifiers, TermName(termName), Ident(TypeName(typeName)), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelParameter(ModelParameterTerm(termName), ModelParameterType(typeName, List())) :: getParameters(xs)
    }
    case ValDef(modifiers, TermName(termName), AppliedTypeTree(Ident(TypeName(typeName)), typeParameters), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ModelParameter(ModelParameterTerm(termName), ModelParameterType(typeName, getTypeParameters(typeParameters))) :: getParameters(xs)
    }
    case x :: xs => getParameters(xs)
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

  private def getRepresentation(ast: List[Tree], termName: String): Option[ModelRep] = {
    def extractLiterals(list: List[Tree]): List[String] = list match {
      case Literal(Constant(x: String)) :: xs => x :: extractLiterals(xs)
      case x :: xs => extractLiterals(xs)
      case Nil => Nil
    }

    ast match {
      case ValDef(modifiers, TermName(t), typeTree, Apply(Ident(TermName("GenObjectRep")), List())) :: xs if t == termName => {
        Some(ModelObjectRep(List()))
      }
      case ValDef(modifiers, TermName(t), typeTree, Apply(Ident(TermName("GenObjectRep")), List(Apply(Ident(TermName("List")), literals)))) :: xs if t == termName => {
        Some(ModelObjectRep(extractLiterals(literals)))
      }
      case ValDef(modifiers, TermName(t), typeTree, Ident(TermName("GenParameterRep"))) :: xs if t == termName => {
        Some(ModelParameterRep)
      }
      case x :: xs => getRepresentation(xs, termName)
      case Nil => Some(ModelObjectRep(List())) // TODO: Look at parent's field
    }
  }

  private def getCustomFields(ast: List[Tree], suffix: String): Map[String, Tree] = {
    ast match {
      case ValDef(modifiers, TermName(t), typeTree, eq) :: xs if t.endsWith(suffix) => {
        Map(t -> eq) ++ getCustomFields(xs, suffix)
      }
      case x :: xs => getCustomFields(xs, suffix)
      case Nil => Map()
    }
  }

  private def traverseForModels(ast: Tree, packageName: String): List[Model] = {
    ast match {
      case Block(body, expr) => {
        body.flatMap(childAst => traverseForModels(childAst, packageName))
      }
      case ModuleDef(modifiers, TermName(objectName), Template(parents, self, body)) => {
        body.flatMap(childAst => traverseForModels(childAst, s"$packageName.$objectName"))
      }
      case ClassDef(modifiers, TypeName(name), typeDef, Template(parents, self, body)) if parentExists(parents, "GenReader") || parentExists(parents, "GenWriter") => {
        val parameters = getParameters(body)
        List(
          Model(
            ModelName(name),
            ModelPackage(packageName),
            parameters,
            ModelDefaultParameterValues(getDefaultParameters(body)),
            if (parentExists(parents, "GenReader")) getRepresentation(body, "readerRep") else None,
            if (parentExists(parents, "GenWriter")) getRepresentation(body, "writerRep") else None,
            getCustomFields(body, "Reader"),
            getCustomFields(body, "Writer")
          )
        )
      }
      case _ => List()
    }
  }

  def generateModelsFor(code: String): List[Model] = {
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    "^(\\s+)?package (.*)".r.findFirstIn(code).map(_.split(" ")(1)) match {
      case Some(packageName) => {
        try {
          val ast = tb.parse(code.replaceFirst("^(\\s+)?package", "//package"))

//          println(showRaw(ast))
          traverseForModels(ast, packageName)
        } catch {
          case e: Throwable => {
            println(e)
            List()
          }
        }
      }
      case None => List()
    }
  }

}
