import sbt._
import Keys._

object Dependencies {

  val commonDependencies: Seq[ModuleID] = Seq(
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "commons-io" % "commons-io" % "2.5",
    "com.opticdev" %% "parser-foundation" % "0.1.2",
    "org.scala-lang" % "scala-reflect" % "2.12.3"
  )

  val trainingDependencies    : Seq[ModuleID] = commonDependencies ++ Seq(
    "com.stripe" %% "brushfire-training" % "0.7.6-SNAPSHOT",
    "com.stripe" %% "brushfire-tree" % "0.7.6-SNAPSHOT",
    "com.eed3si9n" %% "treehugger" % "0.4.3"
  )
  val runtimeDependencies : Seq[ModuleID] = commonDependencies ++ Seq()

}
