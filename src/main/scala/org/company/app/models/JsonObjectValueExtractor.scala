package org.company.app.models

import com.plasmaconduit.json._
import com.plasmaconduit.validation._

object JsonObjectValueExtractor {

  def apply[A: JsReader, F](key: String, missing: F, invalid: JsReader[A]#JsReaderFailure => F, default: Option[A] = None): Map[String, JsValue] => Validation[F, A] = {
    (map) => {
      val res = for {
        valueJ <- map.get(key).fold[Validation[F, JsValue]](Failure(missing))(j => Success(j))
        value <- valueJ.as[A].mapError(invalid)
      } yield value

      (res, default) match {
        case (s @ Success(_), _) => s
        case (Failure(missing), Some(d)) => Success(d)
        case (f @ Failure(_), _) => f
      }
    }
  }

}