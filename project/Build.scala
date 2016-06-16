import sbt._
import sbt.Keys._

import bintray.Plugin._
import bintray.Keys._

import sbtassembly.AssemblyKeys._

object Build extends Build {

  val customBintraySettings = bintrayPublishSettings ++ Seq(
    packageLabels in bintray       := Seq("json", "codegen"),
    bintrayOrganization in bintray := Some("plasmaconduit"),
    repository in bintray          := "releases"
  )

  lazy val root = Project("root", file("."))
    .settings(
      name                        := "jcg",
      organization                := "com.plasmaconduit",
      version                     := "0.3.0",
      scalaVersion                := "2.11.8",
      ivyScala                    := ivyScala.value map { _.copy(overrideScalaVersion = true) },
      licenses                    += ("MIT", url("http://opensource.org/licenses/MIT")),
      scalacOptions               += "-feature",
      scalacOptions               += "-deprecation",
      scalacOptions               += "-unchecked",
      scalacOptions               += "-language:implicitConversions",
      scalacOptions in Test       ++= Seq("-Yrangepos"),
      resolvers                   ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo),
      resolvers                   += "Plasma Conduit Repository" at "http://dl.bintray.com/plasmaconduit/releases",
      libraryDependencies         += "com.plasmaconduit" %% "json" % "0.23.0",
      libraryDependencies         += "com.eed3si9n" %% "treehugger" % "0.4.1",
      libraryDependencies         += "org.specs2" %% "specs2" % "2.3.11" % "test",
      assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
    )

  lazy val traits = Project("traits", file("./traits"))
    .settings(customBintraySettings: _*)
    .settings(
      name                  := "jcg-traits",
      organization          := "com.plasmaconduit",
      version               := "0.3.0",
      crossScalaVersions    := Seq("2.10.4", "2.11.8"),
      licenses              += ("MIT", url("http://opensource.org/licenses/MIT")),
      scalacOptions         += "-feature",
      scalacOptions         += "-deprecation",
      scalacOptions         += "-unchecked",
      scalacOptions in Test ++= Seq("-Yrangepos"),
      libraryDependencies   += "org.specs2" %% "specs2" % "2.3.11" % "test"
    )

  lazy val examples = Project("examples", file("./examples"))
    .settings(
      name                  := "jcg-examples",
      organization          := "com.plasmaconduit",
      version               := "0.3.0",
      scalaVersion          := "2.11.8",
      licenses              += ("MIT", url("http://opensource.org/licenses/MIT")),
      scalacOptions         += "-feature",
      scalacOptions         += "-deprecation",
      scalacOptions         += "-unchecked",
      scalacOptions in Test ++= Seq("-Yrangepos"),
      libraryDependencies   += "com.plasmaconduit" %% "json" % "0.23.0",
      libraryDependencies   += "org.specs2" %% "specs2" % "2.3.11" % "test"
    )
    .dependsOn(traits)

}
