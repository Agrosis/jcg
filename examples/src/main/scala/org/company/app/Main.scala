package org.company.app

import com.plasmaconduit.json.{JsString, JsArray, JsObject}
import org.company.app.models.{PhoneNumber, Item, User}

import json.writers.GenJsWriters._
import json.readers.GenJsReaders._

object Main {

  def main(args: Array[String]): Unit = {
    val user = User(3, "Agro", "sykretz", "agro@jantox.com", List(Item(3, "Portal 2"), Item(4, "Left 4 Dead 2")))

    val payload = JsObject(
      "user" -> user,
      "phoneNumber" -> PhoneNumber("1234567890")
    )

    println(payload)

    val input = JsObject(
      "id" -> 3,
      "name" -> "Portal 2"
    )

    val input2 = JsObject(
      "id" -> 3,
      "username" -> "Agro",
      "password" -> "sykretz",
      "email" -> "agro@jantox.com",
      "items" -> JsArray(JsObject("id" -> 3, "name" -> "Portal 2"), JsObject("id" -> 4, "name" -> "Left 4 Dead 2"))
    )

    val input3 = JsString("1234567890")

    println(input.as[Item])
    println(input2.as[User])
    println(input3.as[PhoneNumber])
  }

}
