package com.plasmaconduit.json.codegen

final case class Model(name: ModelName,
                       modelPackage: ModelPackage,
                       parameters: List[ModelParameter],
                       defaultValues: ModelDefaultParameterValues,
                       writerIgnore: ModelIgnoreParameters,
                       genReader: Boolean,
                       genWriter: Boolean) {

  def fullyQualifiedName: String = s"${modelPackage.value}.${name.value}"

  def writerParameters: List[ModelParameter] = parameters.filter(p => !writerIgnore.terms.contains(p.term.value))

}
final case class ModelPackage(value: String)
final case class ModelName(value: String)

final case class ModelParameter(term: ModelParameterTerm, parameterType: ModelParameterType)
final case class ModelParameterTerm(value: String)
final case class ModelParameterType(value: String, typeParameters: List[ModelParameterType])
final case class ModelDefaultParameterValues(map: Map[String, Any])

final case class ModelIgnoreParameters(terms: List[String])
