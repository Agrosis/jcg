package com.plasmaconduit.json.codegen.traits

sealed trait GenRep
final case class GenObjectRep(ignore: List[String] = List()) extends GenRep
final case object GenParameterRep extends GenRep

