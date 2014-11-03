import sbt._

object Dependencies {
  val resolutionRepos = Seq(
    "Secured Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
    Resolver.sonatypeRepo("snapshots"),
    "Eligosource Releases" at "http://repo.eligotech.com/nexus/content/repositories/eligosource-releases"
  )

  def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")

  def provided(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")

  def unittest(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

  def runtime(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")

  def container(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")

  object V {
    val akka = "2.2.4"
  }

  // Akka
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akka
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % V.akka

  val lmaxDisruptor = "com.lmax" % "disruptor" % "3.3.0"

  // Logging
  val scalalogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"

  // Utils
  val typesafeConfig = "com.typesafe" % "config" % "1.2.1"
  val scalazCore = "org.scalaz" %% "scalaz-core" % "7.0.6"
  val scalaSTM = "org.scala-stm" %% "scala-stm" % "0.7"
  val shapeless = "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full

  // Testing
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % V.akka
  val scalaTest = "org.scalatest" %% "scalatest" % "2.1.6"
  val scalaMock = "org.scalamock" %% "scalamock-scalatest-support" % "3.1.RC1"
  val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.6"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.5"
}
