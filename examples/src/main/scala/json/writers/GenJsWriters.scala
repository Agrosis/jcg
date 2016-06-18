package json.writers {
  import com.plasmaconduit.json._;
  object GenJsWriters extends  {
    implicit lazy val ItemJsWriterImplicit = ItemJsWriter;
    implicit lazy val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter;
    implicit lazy val UserJsWriterImplicit = UserJsWriter;
    object ItemJsWriter extends JsWriter[org.company.app.models.Item] {
      override def write(m: org.company.app.models.Item): JsValue = JsObject(scala.Tuple2("id", m.id), scala.Tuple2("name", JsString(m.name)))
    };
    object PhoneNumberJsWriter extends JsWriter[org.company.app.models.PhoneNumber] {
      override def write(m: org.company.app.models.PhoneNumber): JsValue = JsString(m.value)
    };
    object UserJsWriter extends JsWriter[org.company.app.models.User] {
      override def write(m: org.company.app.models.User): JsValue = JsObject(scala.Tuple2("id", m.id), scala.Tuple2("username", JsString(m.username)), scala.Tuple2("email", JsString(m.email)), scala.Tuple2("items", m.items))
    }
  }
}