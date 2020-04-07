import sbt._
import sbt.Keys._

object HelloCats {
  lazy val base: Project => Project =
    _.settings(Dependencies.scalacOptionSettings)

  lazy val core = project
    .in(file("core"))
    .configure(base)
    .settings(name := "hello-cats-core",
      libraryDependencies ++= Dependencies.core)

  lazy val root = project
    .in(file("."))
    .aggregate(
      core
    )
}

