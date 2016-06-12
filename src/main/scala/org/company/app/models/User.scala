package org.company.app.models

import com.plasmaconduit.json.codegen.GenWriter

case class User(id: Long, email: String, username: String, items: List[String]) extends GenWriter
