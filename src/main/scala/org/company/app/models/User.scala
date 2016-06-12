package org.company.app.models

import com.plasmaconduit.json.codegen.{GenReader, GenWriter}

case class User(id: Long, email: String, username: String, items: List[Item]) extends GenWriter with GenReader
