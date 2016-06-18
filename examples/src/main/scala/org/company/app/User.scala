package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenReader, GenObjectRep, GenWriter}

case class User(id: Int, username: String, password: String, email: String, items: List[Item]) extends GenWriter with GenReader {
  override val writerRep = GenObjectRep(List("password"))
}
