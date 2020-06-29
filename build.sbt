val catsVersion = "2.1.1"
val scalatestVersion = "3.0.5"
val algebraVersion = "2.0.0"
val shapelessVersion = "2.3.3"

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.montokapro",
    name := "fuzzy-set",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % catsVersion,
      "org.typelevel" %% "cats-testkit" % catsVersion % Test,
      "org.typelevel" %% "cats-testkit-scalatest" % "1.0.1" % Test,
      "org.typelevel" %% "discipline-scalatest" % "1.0.1" % Test,
      "org.typelevel" %% "algebra" % algebraVersion,
      "org.typelevel" %% "algebra-laws" % algebraVersion % Test,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
    )
  )
