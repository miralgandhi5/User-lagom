
name := "User-lagom"

organization in ThisBuild := "edu.knoldus"
version in ThisBuild := "1.0-SNAPSHOT"

// the Scala version that will be used for cross-compiled libraries
scalaVersion in ThisBuild := "2.12.4"

val macwire = "com.softwaremill.macwire" %% "macros" % "2.3.0" % "provided"
val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4" % Test

lazy val `user-lagom` = (project in file("."))
  .aggregate(`user-api`, `user-impl`)
lazy val `user-api` = (project in file("user-api"))
  .settings(
    libraryDependencies ++= Seq(
      lagomScaladslApi
    )
  )

lazy val `user-impl` = (project in file("user-impl"))
  .enablePlugins(LagomScala)
  .settings(
    libraryDependencies ++= Seq(
      macwire,
      scalaTest
    )
  )
  .settings(lagomForkedTestSettings: _*)
  .dependsOn(`user-api`)
lagomUnmanagedServices in ThisBuild := Map("external-service" -> "https://jsonplaceholder.typicode.com")
