package org.company.app.models

import com.plasmaconduit.json.codegen.{GenReader, GenWriter}

case class User(id: Long, email: String, username: String, i: List[List[String]], map: Map[String, List[Item]]) extends GenWriter with GenReader
