package json.readers {

import java.time.LocalDateTime

import com.plasmaconduit.json._;
  import com.plasmaconduit.validation._;
  object GenJsReaders extends AnyRef {
    implicit lazy val DateJsReaderImplicit = DateJsReader;
    implicit lazy val DateRangeJsReaderImplicit = DateRangeJsReader;
    implicit lazy val ItemJsReaderImplicit = ItemJsReader;
    implicit lazy val PhoneNumberJsReaderImplicit = PhoneNumberJsReader;
    implicit lazy val UserJsReaderImplicit = UserJsReader;
    implicit lazy val VehicleJsReaderImplicit = VehicleJsReader;
    implicit lazy val CarJsReaderImplicit = CarJsReader;
    implicit lazy val TruckJsReaderImplicit = TruckJsReader;
    implicit lazy val BoatJsReaderImplicit = BoatJsReader;
    object DateJsReader extends JsReader[org.company.app.models.Date] {
      override type JsReaderFailure = DateJsReaderError;
      sealed trait DateJsReaderError extends AnyRef;
      case object DateInvalidJsonType extends DateJsReaderError;
      val dateReader = org.company.app.models.Date.DateLocalTimeReader;
      override def read(value: JsValue): Validation[DateJsReaderError, org.company.app.models.Date] = {
        value.as[LocalDateTime](dateReader).mapError(((_) => DateInvalidJsonType)).map(((f) => org.company.app.models.Date(f)))
      }
    };
    object DateRangeJsReader extends JsReader[org.company.app.models.DateRange] {
      override type JsReaderFailure = DateRangeJsReaderError;
      sealed trait DateRangeJsReaderError extends AnyRef;
      case object DateRangeNotJsonObject extends DateRangeJsReaderError;
      case object DateRangeStartInvalidError extends DateRangeJsReaderError;
      case object DateRangeStartMissingError extends DateRangeJsReaderError;
      case object DateRangeEndInvalidError extends DateRangeJsReaderError;
      case object DateRangeEndMissingError extends DateRangeJsReaderError;
      val startExtractor = JsonObjectValueExtractor[org.company.app.models.Date, DateRangeJsReaderError](key = "start", missing = DateRangeStartMissingError, invalid = ((x) => DateRangeStartInvalidError), default = None);
      val endExtractor = JsonObjectValueExtractor[org.company.app.models.Date, DateRangeJsReaderError](key = "end", missing = DateRangeEndMissingError, invalid = ((x) => DateRangeEndInvalidError), default = None);
      override def read(value: JsValue): Validation[DateRangeJsReaderError, org.company.app.models.DateRange] = {
        value match {
          case JsObject((map @ _)) => startExtractor(map).flatMap(((start) => {
            endExtractor(map).map(((end) => {
              org.company.app.models.DateRange(start = start, end = end)
            }))
          }))
          case _ => Failure(DateRangeNotJsonObject)
        }
      }
    };
    object ItemJsReader extends JsReader[org.company.app.models.Item] {
      override type JsReaderFailure = ItemJsReaderError;
      sealed trait ItemJsReaderError extends AnyRef;
      case object ItemNotJsonObject extends ItemJsReaderError;
      case object ItemIdInvalidError extends ItemJsReaderError;
      case object ItemIdMissingError extends ItemJsReaderError;
      case object ItemNameInvalidError extends ItemJsReaderError;
      case object ItemNameMissingError extends ItemJsReaderError;
      val nameReader = org.company.app.models.Item.ItemNameJsReader;
      val idExtractor = JsonObjectValueExtractor[Int, ItemJsReaderError](key = "id", missing = ItemIdMissingError, invalid = ((x) => ItemIdInvalidError), default = None);
      val nameExtractor = JsonObjectValueExtractor[String, ItemJsReaderError](key = "name", missing = ItemNameMissingError, invalid = ((x) => ItemNameInvalidError), default = None)(nameReader);
      override def read(value: JsValue): Validation[ItemJsReaderError, org.company.app.models.Item] = {
        value match {
          case JsObject((map @ _)) => idExtractor(map).flatMap(((id) => {
            nameExtractor(map).map(((name) => {
              org.company.app.models.Item(id = id, name = name)
            }))
          }))
          case _ => Failure(ItemNotJsonObject)
        }
      }
    };
    object PhoneNumberJsReader extends JsReader[org.company.app.models.PhoneNumber] {
      override type JsReaderFailure = PhoneNumberJsReaderError;
      sealed trait PhoneNumberJsReaderError extends AnyRef;
      case object PhoneNumberInvalidJsonType extends PhoneNumberJsReaderError;
      override def read(value: JsValue): Validation[PhoneNumberJsReaderError, org.company.app.models.PhoneNumber] = {
        value.as[String].mapError(((_) => PhoneNumberInvalidJsonType)).map(((f) => org.company.app.models.PhoneNumber(f)))
      }
    };
    object UserJsReader extends JsReader[org.company.app.models.User] {
      override type JsReaderFailure = UserJsReaderError;
      sealed trait UserJsReaderError extends AnyRef;
      case object UserNotJsonObject extends UserJsReaderError;
      case object UserIdInvalidError extends UserJsReaderError;
      case object UserIdMissingError extends UserJsReaderError;
      case object UserUsernameInvalidError extends UserJsReaderError;
      case object UserUsernameMissingError extends UserJsReaderError;
      case object UserPasswordInvalidError extends UserJsReaderError;
      case object UserPasswordMissingError extends UserJsReaderError;
      case object UserEmailInvalidError extends UserJsReaderError;
      case object UserEmailMissingError extends UserJsReaderError;
      case object UserItemsInvalidError extends UserJsReaderError;
      case object UserItemsMissingError extends UserJsReaderError;
      case object UserLastPurchaseInvalidError extends UserJsReaderError;
      case object UserLastPurchaseMissingError extends UserJsReaderError;
      val idExtractor = JsonObjectValueExtractor[Int, UserJsReaderError](key = "id", missing = UserIdMissingError, invalid = ((x) => UserIdInvalidError), default = None);
      val usernameExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "username", missing = UserUsernameMissingError, invalid = ((x) => UserUsernameInvalidError), default = None);
      val passwordExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "password", missing = UserPasswordMissingError, invalid = ((x) => UserPasswordInvalidError), default = None);
      val emailExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "email", missing = UserEmailMissingError, invalid = ((x) => UserEmailInvalidError), default = None);
      val itemsExtractor = JsonObjectValueExtractor[List[org.company.app.models.Item], UserJsReaderError](key = "items", missing = UserItemsMissingError, invalid = ((x) => UserItemsInvalidError), default = None);
      val lastPurchaseExtractor = JsonObjectValueExtractor[Option[org.company.app.models.Item], UserJsReaderError](key = "lastPurchase", missing = UserLastPurchaseMissingError, invalid = ((x) => UserLastPurchaseInvalidError), default = None);
      override def read(value: JsValue): Validation[UserJsReaderError, org.company.app.models.User] = {
        value match {
          case JsObject((map @ _)) => idExtractor(map).flatMap(((id) => {
            usernameExtractor(map).flatMap(((username) => {
              passwordExtractor(map).flatMap(((password) => {
                emailExtractor(map).flatMap(((email) => {
                  itemsExtractor(map).flatMap(((items) => {
                    lastPurchaseExtractor(map).map(((lastPurchase) => {
                      org.company.app.models.User(id = id, username = username, password = password, email = email, items = items, lastPurchase = lastPurchase)
                    }))
                  }))
                }))
              }))
            }))
          }))
          case _ => Failure(UserNotJsonObject)
        }
      }
    };
    object VehicleJsReader extends JsReader[org.company.app.models.Vehicle] {
      override type JsReaderFailure = VehicleJsReaderError;
      sealed trait VehicleJsReaderError extends AnyRef;
      case object VehicleNotJsonObject extends VehicleJsReaderError;
      case object VehicleTypeInvalidJsonTypeError extends VehicleJsReaderError;
      case object VehicleTypeInvalidError extends VehicleJsReaderError;
      case object VehicleTypeMissingError extends VehicleJsReaderError;
      case object VehicleCarError extends VehicleJsReaderError;
      case object VehicleTruckError extends VehicleJsReaderError;
      case object VehicleBoatError extends VehicleJsReaderError;
      val typeExtractor = JsonObjectValueExtractor[String, VehicleJsReaderError](key = "type", missing = VehicleTypeMissingError, invalid = ((e) => VehicleTypeInvalidJsonTypeError), default = None);
      override def read(value: JsValue): Validation[VehicleJsReaderError, org.company.app.models.Vehicle] = {
        value match {
          case JsObject((map @ _)) => typeExtractor(map).flatMap(((childType) => readFromType(map, childType)))
          case _ => Failure(VehicleNotJsonObject)
        }
      };
      def readFromType(map: Map[String, JsValue], childType: String): Validation[VehicleJsReaderError, org.company.app.models.Vehicle] = {
        val obj = JsObject(map);
        childType match {
          case "Car" => CarJsReader.read(obj).mapError(((e) => VehicleCarError))
          case "Truck" => TruckJsReader.read(obj).mapError(((e) => VehicleTruckError))
          case "Boat" => BoatJsReader.read(obj).mapError(((e) => VehicleBoatError))
          case _ => Failure(VehicleTypeInvalidError)
        }
      }
    };
    object CarJsReader extends JsReader[org.company.app.models.Car] {
      override type JsReaderFailure = CarJsReaderError;
      sealed trait CarJsReaderError extends AnyRef;
      case object CarNotJsonObject extends CarJsReaderError;
      case object CarSeatsInvalidError extends CarJsReaderError;
      case object CarSeatsMissingError extends CarJsReaderError;
      val seatsExtractor = JsonObjectValueExtractor[Int, CarJsReaderError](key = "seats", missing = CarSeatsMissingError, invalid = ((x) => CarSeatsInvalidError), default = None);
      override def read(value: JsValue): Validation[CarJsReaderError, org.company.app.models.Car] = {
        value match {
          case JsObject((map @ _)) => seatsExtractor(map).map(((seats) => {
            org.company.app.models.Car(seats = seats)
          }))
          case _ => Failure(CarNotJsonObject)
        }
      }
    };
    object TruckJsReader extends JsReader[org.company.app.models.Truck] {
      override type JsReaderFailure = TruckJsReaderError;
      sealed trait TruckJsReaderError extends AnyRef;
      case object TruckNotJsonObject extends TruckJsReaderError;
      case object TruckSpaceInvalidError extends TruckJsReaderError;
      case object TruckSpaceMissingError extends TruckJsReaderError;
      val spaceExtractor = JsonObjectValueExtractor[Int, TruckJsReaderError](key = "space", missing = TruckSpaceMissingError, invalid = ((x) => TruckSpaceInvalidError), default = None);
      override def read(value: JsValue): Validation[TruckJsReaderError, org.company.app.models.Truck] = {
        value match {
          case JsObject((map @ _)) => spaceExtractor(map).map(((space) => {
            org.company.app.models.Truck(space = space)
          }))
          case _ => Failure(TruckNotJsonObject)
        }
      }
    };
    object BoatJsReader extends JsReader[org.company.app.models.Boat] {
      override type JsReaderFailure = BoatJsReaderError;
      sealed trait BoatJsReaderError extends AnyRef;
      case object BoatNotJsonObject extends BoatJsReaderError;
      case object BoatSeatsInvalidError extends BoatJsReaderError;
      case object BoatSeatsMissingError extends BoatJsReaderError;
      val seatsExtractor = JsonObjectValueExtractor[Int, BoatJsReaderError](key = "seats", missing = BoatSeatsMissingError, invalid = ((x) => BoatSeatsInvalidError), default = None);
      override def read(value: JsValue): Validation[BoatJsReaderError, org.company.app.models.Boat] = {
        value match {
          case JsObject((map @ _)) => seatsExtractor(map).map(((seats) => {
            org.company.app.models.Boat(seats = seats)
          }))
          case _ => Failure(BoatNotJsonObject)
        }
      }
    }
  }
}