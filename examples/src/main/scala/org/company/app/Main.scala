package org.company.app

import java.time.LocalDateTime

import com.plasmaconduit.json.{JsString, JsArray, JsObject}
import org.company.app.models._

import json.writers.GenJsWriters._
import json.readers.GenJsReaders._

object Main {

  def main(args: Array[String]): Unit = {
    val user = User(3, "Agro", "sykretz", "agro@jantox.com", List(Item(3, "Portal 2"), Item(4, "Left 4 Dead 2")))

    val payload = JsObject(
      "user" -> user,
      "phoneNumber" -> PhoneNumber("1234567890"),
      "hours" -> DateRange(Date(LocalDateTime.now()), Date(LocalDateTime.now()))
    )

    println(payload)

    val input = JsObject(
      "id" -> 3,
      "name" -> "item_Portal 2"
    )

    val input2 = JsObject(
      "id" -> 3,
      "username" -> "Agro",
      "password" -> "sykretz",
      "email" -> "agro@jantox.com",
      "items" -> JsArray(JsObject("id" -> 3, "name" -> "item_Portal 2"), JsObject("id" -> 4, "name" -> "item_Left 4 Dead 2"))
    )

    val input3 = JsString("1234567890")

    val input4 = JsObject(
      "start" -> "2016-06-19T15:04:39.765",
      "end" -> "2016-06-19T15:04:39.765"
    )

    println(input.as[Item])
    println(input2.as[User])
    println(input3.as[PhoneNumber])
    println(input4.as[DateRange])
  }

}