package com.plasmaconduit.json.codegen.model

import scala.reflect.runtime.universe._

sealed trait Model {
  val name: String
  val packageName: String
  val parents: List[String]

  def fullyQualifiedName: String = s"${packageName}.${name}"

  def hasParent(parent: String): Boolean = parents.exists(_ == parent)
}

final case class TraitModel(name: String,
                            packageName: String,
                            parents: List[String],
                            typeField: String) extends Model

final case class ClassModel(name: String,
                            packageName: String,
                            parents: List[String],
                            parameters: List[ClassModelParameter],
                            defaultValues: Map[String, Any],
                            genReaderRep: Option[ModelRep],
                            genWriterRep: Option[ModelRep],
                            customReaders: Map[String, Tree],
                            customWriters: Map[String, Tree],
                            typeName: Option[String]) extends Model

final case class ClassModelParameter(term: String, parameterType: ClassModelParameterType)
final case class ClassModelParameterType(value: String, typeParameters: List[ClassModelParameterType])
