package org.company.app.models.phone

import com.plasmaconduit.json.codegen.{GenReader, GenWriter}

case class PhoneNumber(number: String) extends GenWriter with GenReader
