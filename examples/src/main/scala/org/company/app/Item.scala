package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenObjectRep, GenWriter, GenReader}

case class Item(id: Int, name: String) extends GenReader with GenWriter {
  val readerRep = GenObjectRep()
  val writerRep = GenObjectRep()
}
