/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "chemf"
  val buildVersion = "1.0.1-SNAPSHOT"
  val buildScalaVersion = "2.10.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
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

object Dependencies {
  val scalaz = "org.scalaz"
  val scalazV = "7.0.0"

  val scalaz_core = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect = scalaz %% "scalaz-effect" % scalazV
  val scalaz_concurrent = scalaz %% "scalaz-concurrent" % scalazV
  val scalaz_iteratee = scalaz %% "scalaz-iteratee" % scalazV
  val scalaz_scalacheck = scalaz %% "scalaz-scalacheck-binding" % scalazV % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++
    Seq (libraryDependencies ++= ds) ++
    com.github.retronym.SbtOneJar.oneJarSettings

  lazy val chemf = Project (
    "chemf",
    file("."),
    settings = addDeps (scalaz_core, scalaz_concurrent,
                        scalaz_effect, scalaz_iteratee,
                        scalaz_scalacheck, scalacheck)
  )
}

// vim: set ts=2 sw=2 et:
