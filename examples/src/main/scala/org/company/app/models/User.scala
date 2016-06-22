package org.company.app.models

import com.plasmaconduit.json.codegen.traits._

case class User(id: Int, username: String, password: String, email: String, items: List[Item]) extends GenWriter with GenReader {
  override val writerRep = GenObjectRep(Ignore("password"))
}
