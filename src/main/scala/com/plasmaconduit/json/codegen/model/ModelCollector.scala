package com.plasmaconduit.json.codegen.model

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object ModelCollector {

  private def getParents(parents: List[Tree]): List[String] = parents match {
    case x :: xs => x match {
      case Ident(TypeName(name)) => name :: getParents(xs)
      case _ => getParents(xs)
    }
    case Nil => Nil
  }

  @annotation.tailrec
  private def parentExists(parents: List[Tree], parent: String): Boolean = parents match {
    case x :: xs => x match {
      case Ident(TypeName(name)) if name == parent => true
      case _ => parentExists(xs, parent)
    }
    case Nil => false
  }

  private def getTypeParameters(typeParameters: List[Tree]): List[ClassModelParameterType] = {
    typeParameters.map {
      case Ident(TypeName(name)) => ClassModelParameterType(name, List())
      case AppliedTypeTree(Ident(TypeName(name)), typeParameters) => ClassModelParameterType(name, getTypeParameters(typeParameters))
    }
  }

  private def getParameters(ast: List[Tree]): List[ClassModelParameter] = ast match {
    case ValDef(modifiers, TermName(termName), Ident(TypeName(typeName)), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ClassModelParameter(termName, ClassModelParameterType(typeName, List())) :: getParameters(xs)
    }
    case ValDef(modifiers, TermName(termName), AppliedTypeTree(Ident(TypeName(typeName)), typeParameters), eq) :: xs if modifiers.hasFlag(Flag.CASEACCESSOR) => {
      ClassModelParameter(termName, ClassModelParameterType(typeName, getTypeParameters(typeParameters))) :: getParameters(xs)
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
      case ValDef(modifiers, TermName(t), typeTree, Apply(Ident(TermName("GenObjectRep")), List(Apply(Ident(TermName("Ignore")), literals)))) :: xs if t == termName => {
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
        if (modifiers.hasFlag(Flag.SEALED) && modifiers.hasFlag(Flag.TRAIT)) {
          List(
            TraitModel(
              name = name,
              packageName = packageName,
              parents = getParents(parents)
            )
          )
        } else {
          val parameters = getParameters(body)
          List(
            ClassModel(
              name = name,
              packageName = packageName,
              parents = getParents(parents),
              parameters = parameters,
              defaultValues = getDefaultParameters(body),
              genReaderRep = getRepresentation(body, "readerRep"),
              genWriterRep = getRepresentation(body, "writerRep"),
              customReaders = getCustomFields(body, "Reader"),
              customWriters = getCustomFields(body, "Writer")
            )
          )
        }
      }
      case _ => List()
    }
  }

  def generateModelsFor(code: String): List[Model] = {
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

//    println((
//      Match(
//        Ident(TermName("x")),
//        List(
//          CaseDef(Bind(TermName("i"), Typed(Ident(termNames.WILDCARD), Ident(TypeName("Something")))), EmptyTree, Literal(Constant(3))),
//          CaseDef(Bind(TermName("i"), Typed(Ident(termNames.WILDCARD), Ident(TypeName("Thing")))), EmptyTree, Literal(Constant(7)))
//        )
//      )
//    ))

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
