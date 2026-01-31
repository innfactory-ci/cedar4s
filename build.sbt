import sbt.Keys._

// =============================================================================
// Global Settings
// =============================================================================

val scala213Version = "2.13.16"
val scala212Version = "2.12.20"
val scala3Version = "3.7.0"

val defaultScalaVersion = scala3Version

ThisBuild / organization := "io.github.devnico"
ThisBuild / scalaVersion := defaultScalaVersion
ThisBuild / homepage := Some(url("https://github.com/DevNico/cedar4s"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("DevNico", "Nicolas", "", url("https://github.com/DevNico"))
)
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/DevNico/cedar4s"), "scm:git:git@github.com:DevNico/cedar4s.git")
)

// Dependency versions
val cedarJavaVersion = "4.8.0"
val circeVersion = "0.14.10"
val munitVersion = "1.1.1"
val munitScalacheckVersion = "1.1.0"
val scalacheckVersion = "1.18.1"
val testcontainersVersion = "2.0.3"
val postgresqlVersion = "42.7.9"
val hikariVersion = "7.0.2"
val openTelemetryVersion = "1.44.1"
val otel4sVersion = "0.14.0"
val catsEffectVersion = "3.6.3"
val slf4jVersion = "2.0.16"
val logstashVersion = "8.0"
val logbackVersion = "1.5.15"

lazy val commonSettings = Seq(
  scalacOptions := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq("-Xsource:2.13", "-language:higherKinds")
      case Some((2, 13)) => Seq("-Xsource:3-cross", "-language:higherKinds")
      case Some((3, _))  => Seq("-no-indent")
      case _             => Seq.empty
    }
  },
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % munitVersion % Test,
    "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
    "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val root = (project in file("."))
  .settings(
    name := "cedar4s-root",
    publish / skip := true
  )
  .aggregate(
    types,
    schema,
    core,
    caffeine,
    client,
    codegen,
    bench,
    benchIntegration,
    observabilityAudit,
    observabilityOtel
  )

lazy val types = (project in file("modules/types"))
  .settings(commonSettings)
  .settings(
    name := "cedar4s-types",
    description := "Shared Cedar types (cross-compiled)",
    // Cross-compile for all consumers: sbt plugin (2.12), codegen (2.13), runtime (3.x)
    crossScalaVersions := Seq(scala212Version, scala213Version, scala3Version),
    // Add version-specific source directories for Newtype (opaque types in Scala 3, type alias in Scala 2)
    Compile / unmanagedSourceDirectories ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq((Compile / sourceDirectory).value / "scala-2")
        case Some((3, _)) => Seq((Compile / sourceDirectory).value / "scala-3")
        case _            => Seq.empty
      }
    }
  )

lazy val schema = (project in file("modules/schema"))
  .dependsOn(types)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-schema",
    description := "Cedar schema parser and AST (cross-compiled)",
    // Cross-compile for sbt plugin (2.12) and main code (2.13, 3.x)
    crossScalaVersions := Seq(scala212Version, scala213Version, scala3Version),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "com.cedarpolicy" % "cedar-java" % cedarJavaVersion classifier "uber"
    )
  )

lazy val core = (project in file("modules/core"))
  .dependsOn(types)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-core",
    description := "Cedar auth types and DSL",
    // Cross-compile for Scala 2.13 and 3.x
    crossScalaVersions := Seq(scala213Version, scala3Version)
  )

lazy val caffeine = (project in file("modules/caffeine"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-caffeine",
    description := "Caffeine-based entity cache for cedar4s",
    crossScalaVersions := Seq(scala213Version, scala3Version),
    libraryDependencies ++= Seq(
      "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8"
    )
  )

lazy val client = (project in file("modules/client"))
  .dependsOn(core, schema)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-client",
    description := "Cedar authorization engine wrapper",
    // Cross-compile for Scala 2.13 and 3.x
    crossScalaVersions := Seq(scala213Version, scala3Version),
    libraryDependencies ++= Seq(
      // Use uber classifier which includes native libraries for all platforms
      "com.cedarpolicy" % "cedar-java" % cedarJavaVersion classifier "uber"
    )
  )

lazy val codegen = (project in file("modules/codegen"))
  .dependsOn(schema)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-codegen",
    description := "Cedar schema to Scala code generator",
    // Cross-compile for sbt plugin (2.12) and main code (2.13, 3.x)
    crossScalaVersions := Seq(scala212Version, scala213Version, scala3Version),
    // Need cedar-java for tests since schema module uses it for parsing
    libraryDependencies ++= Seq(
      ("com.cedarpolicy" % "cedar-java" % cedarJavaVersion classifier "uber") % Test
    ),
    // JNI requires flat classloader strategy for tests
    Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
    Test / fork := true
  )

lazy val `sbt-cedar4s` = (project in file("modules/sbt-cedar4s"))
  .enablePlugins(SbtPlugin)
  .dependsOn(codegen)
  .settings(
    name := "sbt-cedar4s",
    description := "SBT plugin for Cedar schema code generation",
    // SBT plugins must be Scala 2.12
    scalaVersion := scala212Version,
    crossScalaVersions := Seq(scala212Version),
    scalacOptions := Seq("-Xsource:2.13", "-language:higherKinds"),
    libraryDependencies ++= Seq(
      // Cedar Java is needed at runtime for schema parsing
      "com.cedarpolicy" % "cedar-java" % cedarJavaVersion classifier "uber"
    ),
    // Disable cross-version conflict warnings for this project since it's
    // a Scala 2.12 sbt plugin in a mixed Scala 2.12/3.x build
    conflictWarning := ConflictWarning.disable,
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++ Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false
  )

lazy val bench = (project in file("modules/bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(client, codegen, caffeine)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-bench",
    description := "JMH benchmarks for cedar4s",
    publish / skip := true,
    // Scala 3 only (depends on client which is Scala 3 only)
    crossScalaVersions := Seq(scala3Version)
  )

lazy val benchIntegration = (project in file("modules/bench-integration"))
  .enablePlugins(JmhPlugin)
  .dependsOn(client)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-bench-integration",
    description := "JMH integration benchmarks with PostgreSQL",
    publish / skip := true,
    crossScalaVersions := Seq(scala3Version),
    libraryDependencies ++= Seq(
      "org.testcontainers" % "testcontainers" % testcontainersVersion,
      "org.testcontainers" % "testcontainers-postgresql" % testcontainersVersion,
      "org.postgresql" % "postgresql" % postgresqlVersion,
      "com.zaxxer" % "HikariCP" % hikariVersion
    )
  )

lazy val observabilityAudit = (project in file("modules/observability-audit"))
  .dependsOn(core, client)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-observability-audit",
    description := "Audit logging for Cedar authorization decisions",
    crossScalaVersions := Seq(scala213Version, scala3Version),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "net.logstash.logback" % "logstash-logback-encoder" % logstashVersion % Optional,
      "ch.qos.logback" % "logback-classic" % logbackVersion % Test
    )
  )

lazy val observabilityOtel = (project in file("modules/observability-otel"))
  .dependsOn(core, client)
  .settings(commonSettings)
  .settings(
    name := "cedar4s-observability-otel",
    description := "OpenTelemetry integration for cedar4s",
    crossScalaVersions := Seq(scala213Version, scala3Version),
    libraryDependencies ++= Seq(
      "io.opentelemetry" % "opentelemetry-api" % openTelemetryVersion,
      "io.opentelemetry" % "opentelemetry-sdk" % openTelemetryVersion % Test,
      "io.opentelemetry" % "opentelemetry-sdk-testing" % openTelemetryVersion % Test
    )
  )
