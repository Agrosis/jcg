package com.plasmaconduit.json.codegen.traits

sealed trait Gen

trait GenWriter extends Gen {
  val writerRep: GenRep = GenObjectRep(Ignore())
}

trait GenReader extends Gen {
  val readerRep: GenRep = GenObjectRep(Ignore())
}
