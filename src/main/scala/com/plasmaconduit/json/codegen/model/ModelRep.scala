package com.plasmaconduit.json.codegen.model

sealed trait ModelRep

final case class ModelObjectRep(ignore: List[String]) extends ModelRep

final case object ModelParameterRep extends ModelRep
