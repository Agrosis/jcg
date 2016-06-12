package com.company.app.models

import com.plasmaconduit.json._
import com.plasmaconduit.validation._

object JsonObjectValueExtractor {

  def apply[A: JsReader, F](key: String,
                            missing: F,
                            invalid: JsReader[A]#JsReaderFailure => F): Map[String, JsValue] => Validation[F, A] =
    (map) => for {
      valueJ <- map.get(key).fold[Validation[F, JsValue]](Failure(missing))(j => Success(j))
      value <- valueJ.as[A].mapError(invalid)
    } yield value

}
