package com.plasmaconduit.json.codegen.traits

sealed trait Gen

trait GenWriter extends Gen {
  val writerIgnoreParameters: List[String] = List()
}
trait GenReader extends Gen
