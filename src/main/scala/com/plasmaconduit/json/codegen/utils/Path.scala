package com.plasmaconduit.json.codegen.utils

case class Path(parts: List[String]) {

  def /(path: Path): Path = {
    copy(parts = parts ++ path.parts)
  }

  override def toString(): String = {
    parts.foldLeft("")((path, part) => {
      val a = if (part.endsWith("/")) part else part + "/"
      path + a
    })
  }

}

object Path {

  implicit def string2Path(s: String): Path = Path(List(s))

  implicit def path2String(p: Path): String = p.toString()

  def packageToPath(packageName: String): Path = string2Path(packageName.replaceAll("\\.", "/"))

}
