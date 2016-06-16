package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenWriter, GenReader}

case class User(id: Int, username: String, email: String, items: List[Item]) extends GenReader with GenWriter
