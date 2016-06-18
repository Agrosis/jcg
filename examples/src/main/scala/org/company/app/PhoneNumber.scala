package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenParameterRep, GenWriter, GenReader}

case class PhoneNumber(value: String) extends GenReader with GenWriter {
  override val readerRep = GenParameterRep
  override val writerRep = GenParameterRep
}
