package json.writers {
  import com.plasmaconduit.json._;
  object GenJsWriters extends AnyRef {
    implicit lazy val DateJsWriterImplicit = DateJsWriter;
    implicit lazy val DateRangeJsWriterImplicit = DateRangeJsWriter;
    implicit lazy val ItemJsWriterImplicit = ItemJsWriter;
    implicit lazy val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter;
    implicit lazy val UserJsWriterImplicit = UserJsWriter;
    implicit lazy val VehicleJsWriterImplicit = VehicleJsWriter;
    implicit lazy val CarJsWriterImplicit = CarJsWriter;
    implicit lazy val TruckJsWriterImplicit = TruckJsWriter;
    implicit lazy val BoatJsWriterImplicit = BoatJsWriter;
    object DateJsWriter extends JsWriter[org.company.app.models.Date] {
      val dateWriter = org.company.app.models.Date.DateLocalTimeWriter;
      override def write(m: org.company.app.models.Date): JsValue = {
        dateWriter.write(m.date)
      }
    };
    object DateRangeJsWriter extends JsWriter[org.company.app.models.DateRange] {
      override def write(m: org.company.app.models.DateRange): JsValue = {
        JsObject(Tuple2("start", m.start), Tuple2("end", m.end))
      }
    };
    object ItemJsWriter extends JsWriter[org.company.app.models.Item] {
      val nameWriter = org.company.app.models.Item.ItemNameJsWriter;
      override def write(m: org.company.app.models.Item): JsValue = {
        JsObject(Tuple2("id", m.id), Tuple2("name", nameWriter.write(m.name)))
      }
    };
    object PhoneNumberJsWriter extends JsWriter[org.company.app.models.PhoneNumber] {
      override def write(m: org.company.app.models.PhoneNumber): JsValue = {
        JsString(m.value)
      }
    };
    object UserJsWriter extends JsWriter[org.company.app.models.User] {
      override def write(m: org.company.app.models.User): JsValue = {
        JsObject(Tuple2("id", m.id), Tuple2("username", JsString(m.username)), Tuple2("email", JsString(m.email)), Tuple2("items", m.items), Tuple2("lastPurchase", JsOption(m.lastPurchase)))
      }
    };
    object VehicleJsWriter extends JsWriter[org.company.app.models.Vehicle] {
      override def write(m: org.company.app.models.Vehicle): JsValue = {
        m match {
          case (c @ (_: org.company.app.models.Car)) => CarJsWriter.write(c)
          case (c @ (_: org.company.app.models.Truck)) => TruckJsWriter.write(c)
          case (c @ (_: org.company.app.models.Boat)) => BoatJsWriter.write(c)
        }
      }
    };
    object CarJsWriter extends JsWriter[org.company.app.models.Car] {
      override def write(m: org.company.app.models.Car): JsValue = {
        JsObject(Tuple2("seats", m.seats), Tuple2("type", "Car"))
      }
    };
    object TruckJsWriter extends JsWriter[org.company.app.models.Truck] {
      override def write(m: org.company.app.models.Truck): JsValue = {
        JsObject(Tuple2("space", m.space), Tuple2("type", "Truck"))
      }
    };
    object BoatJsWriter extends JsWriter[org.company.app.models.Boat] {
      override def write(m: org.company.app.models.Boat): JsValue = {
        JsObject(Tuple2("seats", m.seats), Tuple2("type", "Boat"))
      }
    }
  }
}