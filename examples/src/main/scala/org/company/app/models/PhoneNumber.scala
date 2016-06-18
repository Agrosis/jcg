package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenParameterRep, GenReader, GenWriter}

case class PhoneNumber(value: String) extends GenReader with GenWriter {
  override val readerRep = GenParameterRep
  override val writerRep = GenParameterRep
}
