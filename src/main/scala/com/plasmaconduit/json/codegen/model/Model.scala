package com.plasmaconduit.json.codegen.model

import scala.reflect.runtime.universe._

final case class Model(name: ModelName,
                       modelPackage: ModelPackage,
                       parameters: List[ModelParameter],
                       defaultValues: ModelDefaultParameterValues,
                       genReaderRep: Option[ModelRep],
                       genWriterRep: Option[ModelRep],
                       customReaders: Map[String, Tree],
                       customWriters: Map[String, Tree]) {

  def fullyQualifiedName: String = s"${modelPackage.value}.${name.value}"

}
final case class ModelPackage(value: String)
final case class ModelName(value: String)

final case class ModelParameter(term: ModelParameterTerm, parameterType: ModelParameterType)
final case class ModelParameterTerm(value: String)
final case class ModelParameterType(value: String, typeParameters: List[ModelParameterType])
final case class ModelDefaultParameterValues(map: Map[String, Any])
