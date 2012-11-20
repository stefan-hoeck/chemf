/**                                                               **\
**   Copyright (c) 2012 Center of Organic and Medicinal Chemistry **
**                 Zurich University of Applied Sciences          **
**                 Wädenswil, Switzerland                         **
\**                                                               **/

import sbt._
import Keys._

object BuildSettings {
  import Resolvers._

  val buildOrganization = "chemf"
  val buildVersion = "1.0.0-SNAPSHOT"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    resolvers ++= repos,
    scalacOptions ++= Seq ("-deprecation"),
    initialCommands in console := """
      import scalaz._, Scalaz._
      import chemf._, Element._ 
      import parser.smiles
      import graph.LGraph
      def smi(s: String): Molecule = smiles(s) fold (_ ⇒ LGraph.empty, identity)
      def prettySmiles(s: String): String = smiles(s) fold (_.list mkString "\n", _.shows)
    """
  )

} 

object Resolvers {
 val scalatoolsRepo = "Scala-Tools Maven2 Repository Releases" at
   "http://scala-tools.org/repo-releases"
 val sonatypeRepo = "releases" at
   "http://oss.sonatype.org/content/repositories/releases"
 val repos = Seq (scalatoolsRepo, sonatypeRepo)
}

object Dependencies {
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.4"
  val scalacheckZ = "org.scalaz" %% "scalaz-scalacheck-binding" % "6.0.4"
  val scalacheckZT = scalacheckZ % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.9"
  val scalacheckT = scalacheck % "test"
  val scalazCheckT = Seq(scalaz, scalacheckZT, scalacheckT)
}

object UtilBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: Seq[ModuleID]) =
    BuildSettings.buildSettings ++
    Seq (libraryDependencies ++= ds)

  lazy val chemf = Project (
    "chemf",
    file("."),
    settings = addDeps (scalazCheckT)
  )
}

// vim: set ts=2 sw=2 et:
