package com.plasmaconduit.json.codegen.traits

sealed trait Gen

trait GenWriter extends Gen {
  val writerRep: GenRep
}

trait GenReader extends Gen {
  val readerRep: GenRep
}
