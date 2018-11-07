name := "marvin-root"

organization := "com.opticdev"

scalaVersion := "2.12.3"

version in ThisBuild := "0.2.1"

lazy val common = project.
  settings(Common.settings: _*).
  settings(libraryDependencies ++= Dependencies.commonDependencies)


lazy val runtime = project.
    settings(Common.settings: _*).
    settings(libraryDependencies ++= Dependencies.runtimeDependencies).
    dependsOn(common)


lazy val training = project.
    settings(Common.settings: _*).
    settings(libraryDependencies ++= Dependencies.trainingDependencies).
    dependsOn(runtime)


lazy val root = (project in file(".")).
    settings(Common.settings: _*).
    settings(
      test in assembly := {},
    ).
    aggregate(common, runtime, training)

