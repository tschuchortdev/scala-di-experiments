ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "hkd-di",
    scalacOptions ++= Seq(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-encoding",
      "utf-8", // Specify character encoding used by source files.
      //"-explaintypes", // Explain type errors in more detail.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      "-Wshadow:private-shadow", // A private field (or class parameter) shadows a superclass field.
      "-Wshadow:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      "-Xkind-projector:underscores",
      "-Wnonunit-statement",
      "-Wvalue-discard", // Warn when non-unit return value is discarded (unused)
      "-Wunused:nowarn", // Warn when an @nowarn annotation doesn't suppress anything
      "-experimental",
      "-preview",
      "-Wsafe-init"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.6.1",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-mtl" % "1.5.0",
      "org.typelevel" %% "kittens" % "3.5.0",
      "com.github.tschuchortdev" %% "hkd4s" % "1.1.0"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.3.1",
      "org.typelevel" %% "munit-cats-effect" % "2.2.0",
      "de.lhns" %% "munit-tagless-final" % "0.3.0",
    ).map(_ % Test),
    testFrameworks += new TestFramework("munit.Framework"),
  )
