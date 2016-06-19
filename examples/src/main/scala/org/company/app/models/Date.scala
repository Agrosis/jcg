package org.company.app.models

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.plasmaconduit.json.codegen.traits.{GenParameterRep, GenWriter}
import com.plasmaconduit.json.{JsString, JsValue, JsWriter}

final case class Date(date: LocalDateTime) extends GenWriter {

  override val writerRep = GenParameterRep

  val dateWriter = org.company.app.models.Date.DateLocalTimeWriter

}

object Date {

    object DateLocalTimeWriter extends JsWriter[LocalDateTime] {
      val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
      override def write(l: LocalDateTime): JsValue = {
        JsString(l.format(dateTimeFormatter))
      }
    }

}
