import sbt._
import sbt.Keys._

object Dependencies {

  val scalacOptionSettings = scalacOptions ++= Seq(
    "-language:higherKinds",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xlint"
  )

  object Versions {
    val scala = "2.13.1"
  }

  object Libs {
    val cats = "org.typelevel" %% "cats-core" % "2.1.1"
    val scalatest = "org.scalatest" %% "scalatest" % "3.1.1" % "test"
  }

  val core = Seq(
    Libs.cats,
    Libs.scalatest
  )
}
