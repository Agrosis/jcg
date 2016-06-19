package json.writers {
  import com.plasmaconduit.json._;
  object GenJsWriters extends  {
    implicit lazy val DateJsWriterImplicit = DateJsWriter;
    implicit lazy val DateRangeJsWriterImplicit = DateRangeJsWriter;
    implicit lazy val ItemJsWriterImplicit = ItemJsWriter;
    implicit lazy val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter;
    implicit lazy val UserJsWriterImplicit = UserJsWriter;
    object DateJsWriter extends JsWriter[org.company.app.models.Date] {
      val dateWriter = org.company.app.models.Date.DateLocalTimeWriter;
      override def write(m: org.company.app.models.Date): JsValue = {
        dateWriter.write(m.date)
      }
    };
    object DateRangeJsWriter extends JsWriter[org.company.app.models.DateRange] {
      override def write(m: org.company.app.models.DateRange): JsValue = {
        JsObject(scala.Tuple2("start", m.start), scala.Tuple2("end", m.end))
      }
    };
    object ItemJsWriter extends JsWriter[org.company.app.models.Item] {
      val nameWriter = org.company.app.models.Item.ItemNameJsWriter;
      override def write(m: org.company.app.models.Item): JsValue = {
        JsObject(scala.Tuple2("id", m.id), scala.Tuple2("name", nameWriter.write(m.name)))
      }
    };
    object PhoneNumberJsWriter extends JsWriter[org.company.app.models.PhoneNumber] {
      override def write(m: org.company.app.models.PhoneNumber): JsValue = {
        JsString(m.value)
      }
    };
    object UserJsWriter extends JsWriter[org.company.app.models.User] {
      override def write(m: org.company.app.models.User): JsValue = {
        JsObject(scala.Tuple2("id", m.id), scala.Tuple2("username", JsString(m.username)), scala.Tuple2("email", JsString(m.email)), scala.Tuple2("items", m.items))
      }
    }
  }
}