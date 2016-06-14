package org.company.app.models

import com.plasmaconduit.json._
import com.plasmaconduit.validation._
import org.company.app.models.phone.PhoneNumber

object JsonObjectValueExtractor {

  def apply[A: JsReader, F](key: String, missing: F, invalid: JsReader[A]#JsReaderFailure => F): Map[String, JsValue] => Validation[F, A] =
    (map) => for {
      valueJ <- map.get(key).fold[Validation[F, JsValue]](Failure(missing))(j => Success(j))
      value <- valueJ.as[A].mapError(invalid)
    } yield value

}

import com.plasmaconduit.json._

object GenJsWriters {
  implicit val ItemJsWriterImplicit = ItemJsWriter
  implicit val PhoneNumberJsWriterImplicit = PhoneNumberJsWriter
  implicit val UserJsWriterImplicit = UserJsWriter
  object PhoneNumberJsWriter extends JsWriter[PhoneNumber] {
    override def write(m: PhoneNumber): JsValue = {
      JsObject(("number", JsString(m.number)))
    }
  }
  object UserJsWriter extends JsWriter[User] {
    override def write(m: User): JsValue = {
      JsObject(("id", JsLong(m.id)), ("email", JsString(m.email)), ("password", JsString(m.password)), ("username", JsString(m.username)), ("i", JsArray(m.i.map(x => JsArray(x)))))
    }
  }
  object ItemJsWriter extends JsWriter[Item] {
    override def write(m: Item): JsValue = {
      JsObject(("id", JsLong(m.id)), ("name", JsString(m.name)), ("description", JsString(m.description)))
    }
  }
}

import com.plasmaconduit.json._

import com.plasmaconduit.validation._

object GenJsReaders {
  implicit val ItemJsReaderImplicit = ItemJsReader
  implicit val PhoneNumberJsReaderImplicit = PhoneNumberJsReader
  implicit val UserJsReaderImplicit = UserJsReader
  object ItemJsReader extends JsReader[Item] {
    type JsReaderFailure = ItemJsReaderError
    sealed trait ItemJsReaderError
    case object ItemNotJsonObject extends ItemJsReaderError
    case object ItemIdInvalidError extends ItemJsReaderError
    case object ItemIdMissingError extends ItemJsReaderError
    case object ItemNameInvalidError extends ItemJsReaderError
    case object ItemNameMissingError extends ItemJsReaderError
    case object ItemDescriptionInvalidError extends ItemJsReaderError
    case object ItemDescriptionMissingError extends ItemJsReaderError
    val idExtractor = JsonObjectValueExtractor[Long, ItemJsReaderError](key = "id", missing = ItemIdMissingError, invalid = _ => ItemIdInvalidError)
    val nameExtractor = JsonObjectValueExtractor[String, ItemJsReaderError](key = "name", missing = ItemNameMissingError, invalid = _ => ItemNameInvalidError)
    val descriptionExtractor = JsonObjectValueExtractor[String, ItemJsReaderError](key = "description", missing = ItemDescriptionMissingError, invalid = _ => ItemDescriptionInvalidError)
    override def read(value: JsValue): Validation[ItemJsReaderError, Item] = {
      value match {
        case JsObject(map) => {
          for {
            id <- idExtractor(map)
            name <- nameExtractor(map)
            description <- descriptionExtractor(map)
          } yield Item(id = id, name = name, description = description)
        }
        case _ => Failure(ItemNotJsonObject)
      }
    }
  }
  object PhoneNumberJsReader extends JsReader[PhoneNumber] {
    type JsReaderFailure = PhoneNumberJsReaderError
    sealed trait PhoneNumberJsReaderError
    case object PhoneNumberNotJsonObject extends PhoneNumberJsReaderError
    case object PhoneNumberNumberInvalidError extends PhoneNumberJsReaderError
    case object PhoneNumberNumberMissingError extends PhoneNumberJsReaderError
    val numberExtractor = JsonObjectValueExtractor[String, PhoneNumberJsReaderError](key = "number", missing = PhoneNumberNumberMissingError, invalid = _ => PhoneNumberNumberInvalidError)
    override def read(value: JsValue): Validation[PhoneNumberJsReaderError, PhoneNumber] = {
      value match {
        case JsObject(map) => {
          for (number <- numberExtractor(map))
            yield PhoneNumber(number = number)
        }
        case _ => Failure(PhoneNumberNotJsonObject)
      }
    }
  }
  object UserJsReader extends JsReader[User] {
    type JsReaderFailure = UserJsReaderError
    sealed trait UserJsReaderError
    case object UserNotJsonObject extends UserJsReaderError
    case object UserIdInvalidError extends UserJsReaderError
    case object UserIdMissingError extends UserJsReaderError
    case object UserEmailInvalidError extends UserJsReaderError
    case object UserEmailMissingError extends UserJsReaderError
    case object UserPasswordInvalidError extends UserJsReaderError
    case object UserPasswordMissingError extends UserJsReaderError
    case object UserUsernameInvalidError extends UserJsReaderError
    case object UserUsernameMissingError extends UserJsReaderError
    case object UserIInvalidError extends UserJsReaderError
    case object UserIMissingError extends UserJsReaderError
    val idExtractor = JsonObjectValueExtractor[Long, UserJsReaderError](key = "id", missing = UserIdMissingError, invalid = _ => UserIdInvalidError)
    val emailExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "email", missing = UserEmailMissingError, invalid = _ => UserEmailInvalidError)
    val passwordExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "password", missing = UserPasswordMissingError, invalid = _ => UserPasswordInvalidError)
    val usernameExtractor = JsonObjectValueExtractor[String, UserJsReaderError](key = "username", missing = UserUsernameMissingError, invalid = _ => UserUsernameInvalidError)
    val iExtractor = JsonObjectValueExtractor[List[List[Item]], UserJsReaderError](key = "i", missing = UserIMissingError, invalid = _ => UserIInvalidError)
    override def read(value: JsValue): Validation[UserJsReaderError, User] = {
      value match {
        case JsObject(map) => {
          for {
            id <- idExtractor(map)
            email <- emailExtractor(map)
            password <- passwordExtractor(map)
            username <- usernameExtractor(map)
            i <- iExtractor(map)
          } yield User(id = id, email = email, password = password, username = username, i = i)
        }
        case _ => Failure(UserNotJsonObject)
      }
    }
  }
}

