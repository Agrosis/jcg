package org.company.app.models.phone

import com.plasmaconduit.json.codegen.GenReader
import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

case class PhoneNumber(number: String) extends GenWriter with GenReader
