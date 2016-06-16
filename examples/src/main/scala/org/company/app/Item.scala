package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenWriter, GenReader}

case class Item(id: Int, name: String) extends GenReader with GenWriter
