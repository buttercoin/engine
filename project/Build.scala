import sbt._
import sbt.Tests.Setup
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import scala.Some
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin._
import sbtbuildinfo.Plugin._
import net.virtualvoid.sbt.graph.Plugin._
import de.johoop.jacoco4sbt._
import JacocoPlugin._

object Build extends sbt.Build {
  import Dependencies._

  val buildVersion = "1.12.0-SNAPSHOT"

  val buildSettings = Defaults.coreDefaultSettings ++ graphSettings ++ jacoco.settings ++ Seq(
    initialize ~= { _ => System.setProperty("jsse.enableSNIExtension", "false") }, // deal with handshake errors to github mvn repo's
    organization := "org.buttercoin",
    version := buildVersion,
    scalaVersion := System.getProperty("buttercoin.scalaVersion", "2.10.4"),
    shellPrompt := ShellPrompt.buildShellPrompt,
    resolvers += "Secured Central Repository" at "https://repo1.maven.org/maven2",
    resolvers ++= Dependencies.resolutionRepos,
    externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false),
    scalacOptions := Seq(
      "-encoding", "utf8",
      "-unchecked",
      "-feature",
      "-deprecation",
      "-target:jvm-1.7"),
    jacoco.reportFormats in jacoco.Config := Seq(XMLReport(encoding = "utf-8"), ScalaHTMLReport(withBranchCoverage = true)),
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in (Compile, packageSrc) := false,
    publishConfiguration ~= { config =>
      new PublishConfiguration(config.ivyFile, config.resolverName, config.artifacts.filterKeys(_.`type` != Artifact.SourceType), config.checksums, config.logging, config.overwrite)
    },
    publishMavenStyle := true,
    pomIncludeRepository := {
      _ => false
    }
  )

  val perfTest = taskKey[Unit]("Run perf tests")
  val unitTest = taskKey[Unit]("Run all tests")

  lazy val buttercoinCommon = Project("common", file("common"))
    .settings(buildInfoSettings: _*)
    .settings(graphSettings: _*)
    .settings(jacoco.settings: _*)
    .settings(
      version := buildVersion,
      organization := "org.buttercoin",
      name := "buttercoin-common",
      homepage := Some(new URL("http://www.buttercoin.org")),
      scalaVersion := System.getProperty("buttercoin.scalaVersion", "2.10.4"),
      scalacOptions := Seq(
        "-encoding", "utf8",
        "-unchecked",
        "-feature",
        "-deprecation",
        "-target:jvm-1.7"),
      resolvers += "Secured Central Repository" at "https://repo1.maven.org/maven2",
      resolvers += "Secured Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
      externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false),
      shellPrompt := ShellPrompt.buildShellPrompt,
      sources in doc in Compile := List(),
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys ++= Seq[BuildInfoKey] (
        "buildTime" -> System.currentTimeMillis,
        "gitRevision" -> ("git rev-parse --short HEAD"!!).trim
      ),
      buildInfoPackage := "org.buttercoin.common",
      testOptions += Setup(cl =>
        cl.loadClass("org.slf4j.LoggerFactory").
          getMethod("getLogger", cl.loadClass("java.lang.String")).
          invoke(null, "ROOT")
      ),
      jacoco.reportFormats in jacoco.Config := Seq(XMLReport(encoding = "utf-8"), ScalaHTMLReport(withBranchCoverage = true)),
      publishArtifact in (Compile, packageDoc) := false,
      publishArtifact in (Test, packageBin) := true,
      publishArtifact in (Test, packageDoc) := false,
      publishMavenStyle := true,
      pomIncludeRepository := {
        _ => false
      },
      libraryDependencies ++=
        compile(typesafeConfig, scalazCore, scalalogging, log4jApi, log4jToSlf4j, akkaActor)
        ++ unittest(logbackClassic, scalaTest, scalaCheck, akkaTestkit)
    )

  lazy val root = Project("root", file("."))
    .settings(buildSettings: _*)
    .settings(
      publishLocal := { },
      publish := { },
      publishM2 := { },
      fork in run := true,
      run in Compile <<= (run in Compile in core),
      perfTest in Test <<= (test in Test in perfTesting),
      unitTest in Test <<= (test in Test in core)
    ).aggregate(core)

  lazy val models = Project("models", file("models"))
    .settings(buildSettings: _*)
    .settings(
      packagedArtifacts := Map.empty,
      libraryDependencies ++=
        compile(scalaSTM) ++ unittest(scalaTest)
      ).aggregate(buttercoinCommon).dependsOn(buttercoinCommon % "compile->compile;test->test")

  lazy val core = Project("core", file("core"))
    .settings(buildSettings: _*)
    .settings(buildInfoSettings: _*)
    .settings(assemblySettings: _*)
    .settings(addArtifact(artifact in (Compile, assembly), assembly): _*)
    .settings(
      libraryDependencies ++=
        compile(akkaSlf4j, eventsourcedCore, lmaxDisruptor, scopt,
          logbackClassic, logstashEncoder, janino, shapeless) ++
        unittest(akkaTestkit, scalaMock, scalaTest, scalaCheck),
      name := "jersey",
      fork in run := true,
      baseDirectory in run := file(System.getProperty("user.dir")),
      mainClass := Some("org.buttercoin.jersey.Jersey"),
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys ++= Seq[BuildInfoKey](
        "buildTime" -> System.currentTimeMillis,
        "gitRevision" -> ("git rev-parse --short HEAD" !!).trim
      ),
      buildInfoPackage := "org.buttercoin.jersey",
      mergeStrategy in assembly <<= (mergeStrategy in assembly) {
        (old) => {
          case PathList("META-INF", "maven", xs @ _*) => MergeStrategy.first
          case x => old(x)
        }
      },
      artifact in (Compile, assembly) ~= { art =>
        art.copy(`classifier` = Some("assembly"))
      }
    ).aggregate(buttercoinCommon, models).dependsOn(buttercoinCommon % "compile->compile;test->test", models)

  lazy val perfTesting = Project("perf-testing", file("perf-testing"))
    .settings(buildSettings: _*)
    .settings(
      libraryDependencies ++= unittest(scalaMeter),
      parallelExecution in Test := false,
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      name := "jersey-perf-tests",
      logBuffered := false
    ).dependsOn(core)
}

// Shell prompt which show the current project and git branch
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }

  val buildShellPrompt = {
    val LGREEN = "\033[1;32m"
    val LBLUE = "\033[01;34m"

    (state: State) => {
      val currProject = Project.extract(state).currentProject.id
      if (System.getProperty("sbt.nologformat", "false") != "true") {
        def currBranch = (
          ("git symbolic-ref --short HEAD" lines_! devnull headOption)
            getOrElse "-" stripPrefix "## "
          )

        "%s%s%s:%s%s%s » ".format(LBLUE, currProject, scala.Console.WHITE, LGREEN, currBranch, scala.Console.RESET)
      }
      else {
        "%s%s%s » ".format(LBLUE, currProject, scala.Console.RESET)
      }
    }
  }
}
