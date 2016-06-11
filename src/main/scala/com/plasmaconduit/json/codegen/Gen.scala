package com.plasmaconduit.json.codegen

sealed trait Gen

trait GenWriter extends Gen
trait GenReader extends Gen
