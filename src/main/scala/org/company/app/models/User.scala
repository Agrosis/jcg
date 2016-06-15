package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

case class User(id: Long, email: String, password: String, username: String, i: List[List[String]]) extends GenWriter with GenReader {

  override val writerIgnoreParameters = List("password")

}
