import sbt._
import Keys._

object Common {

  val settings: Seq[Def.Setting[_]] = Seq(
    scalaVersion := "2.12.3",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"), //, "-Xmx2G"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    resolvers += Opts.resolver.mavenLocalFile,
    //    copyDepTask,
    resolvers ++= Seq(DefaultMavenRepository,
      Resolver.defaultLocal,
      Resolver.mavenLocal
    )
  )
}