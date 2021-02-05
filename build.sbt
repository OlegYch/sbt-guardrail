enablePlugins(SbtPlugin)

name := "sbt-guardrail"
organization in ThisBuild := "com.twilio"
description := "Principled code generation from OpenAPI specifications, sbt plugin"
homepage in ThisBuild := Some(url("https://github.com/twilio/sbt-guardrail"))
licenses in ThisBuild += ("MIT", url("https://github.com/twilio/guardrail/blob/master/LICENSE"))

scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/twilio/sbt-guardrail"),
    "scm:git@github.com:twilio/sbt-guardrail.git"
  )
)

developers in ThisBuild := List(
  Developer(
    id = "blast_hardcheese",
    name = "Devon Stewart",
    email = "blast@hardchee.se",
    url = url("http://hardchee.se/")
  )
)

scalaVersion in ThisBuild := "2.12.12"
scalacOptions in ThisBuild ++= List("-feature", "-Xexperimental")

// Versioning
enablePlugins(GitBranchPrompt)
enablePlugins(GitVersioning)
git.useGitDescribe := true

// Release
publishMavenStyle in ThisBuild := true

addCommandAlias(
  "publishSonatype",
  "; set publishTo := sonatypePublishToBundle.value ; set publishTo in core := sonatypePublishToBundle.value ; core/publish ; publish"
)

scriptedLaunchOpts := { scriptedLaunchOpts.value ++
  Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
}

scriptedDependencies := {
  def use[A](@deprecated("unused", "") x: A*): Unit = () // avoid unused warnings
  val analysis = (Keys.compile in Test).value
  val pubPlug = (publishLocal).value
  val pubCore = (core/publishLocal).value
  use(analysis, pubPlug, pubCore)
}

lazy val root = (project in file("."))
  .dependsOn(core)

lazy val core = (project in file("modules/core"))
