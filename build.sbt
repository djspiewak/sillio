// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "com.codecommit"
ThisBuild / organizationName := "Daniel Spiewak"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("djspiewak", "Daniel Spiewak")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

val Scala3 = "3.3.0"
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / scalaVersion := Scala3 // the default Scala

ThisBuild / tlCiReleaseBranches := Seq()
ThisBuild / tlCiReleaseTags := false

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "sillio",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin)
