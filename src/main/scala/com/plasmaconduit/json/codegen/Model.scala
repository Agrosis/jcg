package com.plasmaconduit.json.codegen

final case class Model(name: ModelName, fields: List[ModelField], genReader: Boolean, genWriter: Boolean)
final case class ModelName(value: String)

final case class ModelField(term: ModelFieldTerm, fieldType: ModelFieldType)
final case class ModelFieldTerm(value: String)
final case class ModelFieldType(value: String, typeParameters: List[ModelFieldType])
