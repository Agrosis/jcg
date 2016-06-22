package com.plasmaconduit.json.codegen.traits

sealed trait GenRep
final case class GenObjectRep(ignore: Ignore) extends GenRep
final case object GenParameterRep extends GenRep

final case class Ignore(i: String*)

