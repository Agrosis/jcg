package org.company.app.models

import com.plasmaconduit.json.codegen.{GenReader, GenWriter}

case class User(id: Long = 0, email: String, password: String, username: String = "Hello", i: List[List[String]]) extends GenWriter with GenReader {

  def method(x: Int): Int = {
    3
  }

}
