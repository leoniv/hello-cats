import sbt._
import sbt.Keys._

object HelloCats {
  lazy val base: Project => Project =
    _.settings(Dependencies.scalacOptionSettings)

  lazy val core = project
    .in(file("core"))
    .configure(base)
    .settings(
      name := "hello-cats-core",
      libraryDependencies ++= Dependencies.core
    )

  lazy val effect = project
    .in(file("effect"))
    .configure(base)
    .settings(
      name := "hello-cats-effect",
      libraryDependencies ++= Dependencies.core,
      libraryDependencies ++= Dependencies.catsEffect
    )

  lazy val effect_cc = project
    .in(file("effect_cc"))
    .configure(base)
    .settings(
      name := "hello-cats-effect-copy-contents",
      libraryDependencies ++= Dependencies.core,
      libraryDependencies ++= Dependencies.catsEffect
    )

  lazy val effect_echo_srv = project
    .in(file("effect_echo_srv"))
    .configure(base)
    .settings(
      name := "hello-cats-effect-echo-server",
      libraryDependencies ++= Dependencies.core,
      libraryDependencies ++= Dependencies.catsEffect
    )

  lazy val effect_async_echo_srv = project
    .in(file("effect_async_echo_srv"))
    .configure(base)
    .settings(
      name := "hello-cats-effect-async-echo-server",
      libraryDependencies ++= Dependencies.core,
      libraryDependencies ++= Dependencies.catsEffect
    )

  lazy val fpInScala = project
    .in(file("fp-in-scala"))
    .settings(
      name := "fp-in-scala",
      libraryDependencies ++= Dependencies.fpInScala
    )

  lazy val root = project
    .in(file("."))
    .aggregate(
      core,
      effect,
      effect_cc,
      fpInScala
    )
}
