package org.company.app.models

import com.plasmaconduit.json.codegen.traits.GenWriter

final case class DateRange(start: Date, end: Date) extends GenWriter

