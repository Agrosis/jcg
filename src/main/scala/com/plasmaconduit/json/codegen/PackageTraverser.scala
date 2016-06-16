package com.plasmaconduit.json.codegen

import java.io.File

import com.plasmaconduit.json.codegen.utils.Path
import com.plasmaconduit.json.codegen.utils.Path._

object PackageTraverser {

  def getAllClassesInPackage(baseDirectory: Path, sourceDir: Path, packageName: String): List[File] = {
    traverse(new File(baseDirectory / sourceDir / packageToPath(packageName)))
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
