package org.company.app.models

import com.plasmaconduit.json.codegen.{GenReader, GenWriter}

case class User(id: Long, email: String, password: String, username: String, i: List[List[Item]]) extends GenWriter with GenReader
