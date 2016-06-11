package org.company.app.models

import com.plasmaconduit.json.codegen.GenWriter

case class Item(id: Long, name: String, description: String) extends GenWriter
