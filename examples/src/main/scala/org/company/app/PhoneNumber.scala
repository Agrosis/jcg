package org.company.app

import com.plasmaconduit.json.codegen.traits.{GenWriter, GenReader}

case class PhoneNumber(value: String) extends GenReader with GenWriter
