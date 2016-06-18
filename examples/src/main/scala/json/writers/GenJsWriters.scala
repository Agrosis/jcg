package json.writers {
  import com.plasmaconduit.json._;
  object GenJsWriters extends  {
    implicit lazy val ItemJsWriterImplicit = ItemJsWriter;
    implicit lazy val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter;
    implicit lazy val UserJsWriterImplicit = UserJsWriter;
    object ItemJsWriter extends JsWriter[org.company.app.Item] {
      override def write(m: org.company.app.Item): JsValue = JsObject(scala.Tuple2("id", m), scala.Tuple2("name", JsString(m.name)))
    };
    object PhoneNumberJsWriter extends JsWriter[org.company.app.PhoneNumber] {
      override def write(m: org.company.app.PhoneNumber): JsValue = JsString(m.value)
    };
    object UserJsWriter extends JsWriter[org.company.app.User] {
      override def write(m: org.company.app.User): JsValue = JsObject(scala.Tuple2("id", m), scala.Tuple2("username", JsString(m.username)), scala.Tuple2("email", JsString(m.email)), scala.Tuple2("items", JsArray(m.items)))
    }
  }
}