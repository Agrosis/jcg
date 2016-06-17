package json.readers

import com.plasmaconduit.json._

import com.plasmaconduit.validation._

object GenJsReaders {
  implicit val ItemJsReaderImplicit = ItemJsReader
  implicit val PhoneNumberJsReaderImplicit = PhoneNumberJsReader
  implicit val UserJsReaderImplicit = UserJsReader
  object ItemJsReader extends JsReader[org.company.app.Item] {
    type JsReaderFailure = ItemJsReaderError
    sealed trait ItemJsReaderError
    case object ItemNotJsonObject extends ItemJsReaderError
    case object ItemIdInvalidError extends ItemJsReaderError
    case object ItemIdMissingError extends ItemJsReaderError
    case object ItemNameInvalidError extends ItemJsReaderError
    case object ItemNameMissingError extends ItemJsReaderError
    val idExtractor = JsonObjectValueExtractor[Int, ItemJsReaderError](key = "id", missing = ItemIdMissingError, invalid = _ => ItemIdInvalidError, default = None)
    val nameExtractor = JsonObjectValueExtractor[String, ItemJsReaderError](key = "name", missing = ItemNameMissingError, invalid = _ => ItemNameInvalidError, default = None)
    override def read(value: JsValue): Validation[ItemJsReaderError, org.company.app.Item] = {
      value match {
        case JsObject(map) => {
          for {
            id <- idExtractor(map)
            name <- nameExtractor(map)
          } yield org.company.app.Item(id = id, name = name)
        }
        case _ => Failure(ItemNotJsonObject)
      }
    }
  }
  object PhoneNumberJsReader extends JsReader[org.company.app.PhoneNumber] {
    type JsReaderFailure = PhoneNumberJsReaderError
    sealed trait PhoneNumberJsReaderError
    case object PhoneNumberInvalidJsonType extends PhoneNumberJsReaderError
    override def read(value: JsValue): Validation[PhoneNumberJsReaderError, org.company.app.PhoneNumber] = {
      value.as[String].mapError(_ => PhoneNumberInvalidJsonType).map(x => org.company.app.PhoneNumber(x))
    }
  }
  object UserJsReader extends JsReader[org.company.app.User] {
    type JsReaderFailure = UserJsReaderError
    sealed trait UserJsReaderError
    case object UserNotJsonObject extends UserJsReaderError
    case object UserIdInvalidError extends UserJsReaderError
    case object UserIdMissingError extends UserJsReaderError
    case object UserUsernameInvalidError extends UserJsReaderError
    case object UserUsernameMissingError extends UserJsReaderError
    case object UserPasswordInvalidError extends UserJsReaderError
    case object UserPasswordMissingError extends UserJsReaderError
    case object UserEmailInvalidError extends UserJsReaderError
    case object UserEmailMissingError extends UserJsReaderError
    case object UserItemsInvalidError extends UserJsReaderError
    case object UserItemsMissingError extends UserJsReaderError
    val idExtractor = JsonObjectValueExtractor[Int, UserJsReaderError](key = "id", missing = UserIdMissingError, invalid = _ => UserIdInvalidError, default = None)
    val usernameExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "username", missing = UserUsernameMissingError, invalid = _ => UserUsernameInvalidError, default = None)
    val passwordExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "password", missing = UserPasswordMissingError, invalid = _ => UserPasswordInvalidError, default = None)
    val emailExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "email", missing = UserEmailMissingError, invalid = _ => UserEmailInvalidError, default = None)
    val itemsExtractor = JsonObjectValueExtractor[List[org.company.app.Item], UserJsReaderError](key = "items", missing = UserItemsMissingError, invalid = _ => UserItemsInvalidError, default = None)
    override def read(value: JsValue): Validation[UserJsReaderError, org.company.app.User] = {
      value match {
        case JsObject(map) => {
          for {
            id <- idExtractor(map)
            username <- usernameExtractor(map)
            password <- passwordExtractor(map)
            email <- emailExtractor(map)
            items <- itemsExtractor(map)
          } yield org.company.app.User(id = id, username = username, password = password, email = email, items = items)
        }
        case _ => Failure(UserNotJsonObject)
      }
    }
  }
}