package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenParameterRep, GenWriter, GenReader}

case class PhoneNumber(value: String) extends GenReader with GenWriter {
  val readerRep = GenParameterRep
  val writerRep = GenParameterRep
}
