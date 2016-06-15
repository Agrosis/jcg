package com.plasmaconduit.json.codegen.traits

sealed trait Gen

trait GenWriter extends Gen
trait GenReader extends Gen
