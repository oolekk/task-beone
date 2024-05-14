ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val common = (project in file("common"))
  .settings(
    name := "common",
    libraryDependencies ++= Dependencies.common
  )

lazy val application = (project in file("application"))
  .settings(
    name := "application",
    libraryDependencies ++= Dependencies.application
  ).dependsOn(common)

lazy val client = (project in file("client"))
  .settings(
    name := "client"
  ).dependsOn(common)

lazy val root = (project in file("."))
  .aggregate(common, application, client)
  .settings(
    name := "recruitment-task-beone",
    run := (application / Compile / run).evaluated,
    Global / cancelable := false
  )


