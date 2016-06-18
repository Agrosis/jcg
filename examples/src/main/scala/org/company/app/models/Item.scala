package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenObjectRep, GenReader, GenWriter}

case class Item(id: Int, name: String) extends GenReader with GenWriter {
  override val readerRep = GenObjectRep()
  override val writerRep = GenObjectRep()
}
