package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenReader, GenObjectRep, GenWriter}

case class User(id: Int, username: String, password: String, email: String, items: List[Item]) extends GenWriter with GenReader {
  val writerRep = GenObjectRep(List("password"))
  val readerRep = GenObjectRep()
}
