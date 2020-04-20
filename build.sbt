import HelloCats._

scalaVersion in ThisBuild := Dependencies.Versions.scala

lazy val core = HelloCats.core
lazy val effect = HelloCats.effect
lazy val root = HelloCats.root
lazy val effect_cc = HelloCats.effect_cc
