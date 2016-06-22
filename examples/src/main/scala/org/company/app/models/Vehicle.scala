package org.company.app.models

import com.plasmaconduit.json.codegen.traits.GenWriter

sealed trait Vehicle extends GenWriter

final case class Car(seats: Int) extends Vehicle with GenWriter

final case class Truck(space: Int) extends Vehicle with GenWriter

final case class Boat(seats: Int) extends Vehicle with GenWriter
