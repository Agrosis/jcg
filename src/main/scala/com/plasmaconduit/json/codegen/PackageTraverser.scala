package com.plasmaconduit.json.codegen

import java.io.File

object PackageTraverser {

  def getAllClassesInPackage(baseDirectory: String, packageName: String): List[File] = {
    val directory = s"$baseDirectory/src/main/scala/${packageName.replaceAll("\\.", "/")}"
    traverse(new File(directory))
  }

  private def traverse(file: File): List[File] = {
    file.listFiles().toList.flatMap({
      case f if f.isDirectory() => traverse(f)
      case f => {
        Seq(f)
      }
    })
  }

}
