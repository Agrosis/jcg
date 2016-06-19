package org.company.app.models

import com.plasmaconduit.json.{JsReader, JsString, JsValue, JsWriter}
import com.plasmaconduit.json.codegen.traits.{GenObjectRep, GenReader, GenWriter}
import com.plasmaconduit.validation.{Success, Failure, Validation}

case class Item(id: Int, name: String) extends GenReader with GenWriter {
  val nameWriter = org.company.app.models.Item.ItemNameJsWriter
  val nameReader = org.company.app.models.Item.ItemNameJsReader
}

object Item {

  object ItemNameJsWriter extends JsWriter[String] {
    override def write(a: String): JsValue = {
      JsString(s"item_$a")
    }
  }

  object ItemNameJsReader extends JsReader[String] {
    override type JsReaderFailure = ItemNameJsReaderError

    sealed trait ItemNameJsReaderError
    case object InvalidName extends ItemNameJsReaderError
    case object NotString extends ItemNameJsReaderError

    override def read(value: JsValue): Validation[ItemNameJsReader.JsReaderFailure, String] = {
      value match {
        case JsString(name) if name.startsWith("item_") => Success(name.substring(5))
        case JsString(_) => Failure(InvalidName)
        case _ => Failure(NotString)
      }
    }
  }

}
