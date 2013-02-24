/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

import sbt._
import Keys._

object BuildSettings {
  import Resolvers._

  val buildOrganization = "chemf"
  val buildVersion = "1.0.1-SNAPSHOT"
  val buildScalaVersion = "2.10.0"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    resolvers ++= repos,
    exportJars := true,
    scalacOptions ++= Seq ("-deprecation", "-feature", "-language:postfixOps",
      "-language:higherKinds"),
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
  val scalaz_core = "org.scalaz" %% "scalaz-core" % "7.0.0-M7"
  val scalaz_effect = "org.scalaz" %% "scalaz-effect" % "7.0.0-M7"
  val scalaz_iteratee = "org.scalaz" %% "scalaz-iteratee" % "7.0.0-M7"
  val scalaz_scalacheck =
    "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0-M7"
  val scalaz_scalacheckT = scalaz_scalacheck % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0"
  val scalacheckT = scalacheck % "test"
}

object UtilBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: Seq[ModuleID]) =
    BuildSettings.buildSettings ++
    Seq (libraryDependencies ++= ds) ++
    com.github.retronym.SbtOneJar.oneJarSettings

  lazy val chemf = Project (
    "chemf",
    file("."),
    settings = addDeps (
                 Seq(scalaz_core, scalaz_effect, scalaz_iteratee,
                     scalaz_scalacheckT, scalacheckT)
               )
  )
}

// vim: set ts=2 sw=2 et:
