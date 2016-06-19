package org.company.app.models

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.plasmaconduit.json.codegen.traits.{GenReader, GenParameterRep, GenWriter}
import com.plasmaconduit.json.{JsReader, JsString, JsValue, JsWriter}
import com.plasmaconduit.validation.{Failure, Success, Validation}

final case class Date(date: LocalDateTime) extends GenWriter with GenReader {

  override val writerRep = GenParameterRep
  override val readerRep = GenParameterRep

  val dateWriter = org.company.app.models.Date.DateLocalTimeWriter
  val dateReader = org.company.app.models.Date.DateLocalTimeReader

}

final case class DateRange(start: Date, end: Date) extends GenWriter with GenReader

object Date {

  object DateLocalTimeWriter extends JsWriter[LocalDateTime] {
    val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override def write(l: LocalDateTime): JsValue = {
      JsString(l.format(dateTimeFormatter))
    }
  }

  object DateLocalTimeReader extends JsReader[LocalDateTime] {
    val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override type JsReaderFailure = DateLocalTimeReaderError

    sealed trait DateLocalTimeReaderError
    case object NotJsonString extends DateLocalTimeReaderError

    override def read(value: JsValue): Validation[DateLocalTimeReader.JsReaderFailure, LocalDateTime] = {
      value match {
        case JsString(s) => Success(LocalDateTime.parse(s, dateTimeFormatter))
        case _ => Failure(NotJsonString)
      }
    }
  }

}
