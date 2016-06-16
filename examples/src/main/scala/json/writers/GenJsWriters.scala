package $outputPackage.writers

import com.plasmaconduit.json._

object GenJsWriters {
  implicit val ItemJsWriterImplicit = ItemJsWriter
  implicit val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter
  implicit val UserJsWriterImplicit = UserJsWriter
  object ItemJsWriter extends JsWriter[org.company.app.Item] {
    override def write(m: org.company.app.Item): JsValue = {
      JsObject(("id", m.id), ("name", JsString(m.name)))
    }
  }
  object PhoneNumberJsWriter extends JsWriter[org.company.app.PhoneNumber] {
    override def write(m: org.company.app.PhoneNumber): JsValue = {
      JsObject(("value", JsString(m.value)))
    }
  }
  object UserJsWriter extends JsWriter[org.company.app.User] {
    override def write(m: org.company.app.User): JsValue = {
      JsObject(("id", m.id), ("username", JsString(m.username)), ("email", JsString(m.email)), ("items", JsArray(m.items)))
    }
  }
}