package org.company.app.models

import com.plasmaconduit.json.codegen.traits.{GenReader, GenWriter}

sealed trait Vehicle extends GenWriter with GenReader

final case class Car(seats: Int) extends Vehicle with GenWriter with GenReader

final case class Truck(space: Int) extends Vehicle with GenWriter with GenReader

final case class Boat(seats: Int) extends Vehicle with GenWriter with GenReader
