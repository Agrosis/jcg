package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

sealed trait Vehicle extends GenWriter with GenReader {
  val typeField = "vehicleType"
}

final case class Car(seats: Int) extends Vehicle with GenWriter with GenReader {
  val typeName = "car"
}

final case class Truck(space: Int) extends Vehicle with GenWriter with GenReader {
  val typeName = "truck"
}

final case class Boat(seats: Int) extends Vehicle with GenWriter with GenReader {
  val typeName = "boat"
}
