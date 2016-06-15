package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

case class Item(id: Long, name: String, description: String) extends GenWriter with GenReader
