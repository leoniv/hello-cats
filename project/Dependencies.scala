import sbt._
import sbt.Keys._

object Dependencies {

  val scalacOptionSettings = scalacOptions ++= Seq(
    "-language:higherKinds",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Yrangepos"
  )

  object Versions {
    val scala = "2.13.1"
    val specs2 = "4.8.3"
  }

  object Libs {
    val cats = "org.typelevel" %% "cats-core" % "2.1.1"
    val scalatest = "org.scalatest" %% "scalatest" % "3.1.1" % "test"
    val catsEffect = "org.typelevel" %% "cats-effect" % "2.1.1"
    val specs2 = "org.specs2" %% "specs2-core" % Versions.specs2
  }

  val core = Seq(
    Libs.cats,
    Libs.scalatest
  )

  val catsEffect = Seq(
    Libs.catsEffect
  )

  val fpInScala = Seq(
    Libs.specs2 % Test
    )
}
