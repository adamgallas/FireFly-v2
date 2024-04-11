import sbt.Keys.unmanagedJars
import sbt.file

ThisBuild / version      := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.FireFlyv2"
ThisBuild / scalacOptions += "-target:jvm-1.8"

// SpinalHDL
//val spinalVersion = "1.8.0b"
val spinalVersion = "1.9.0"
val spinalCore    = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib     = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin(
  "com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion
)

// djl for AI & array manipulation
val djlCore    = "ai.djl" % "api" % "0.20.0"
val djlBackend = "ai.djl.pytorch" % "pytorch-engine" % "0.20.0"

val snakeYaml = "org.yaml" % "snakeyaml" % "1.33"

lazy val FireFlyv2 = (project in file("."))
  .settings(
    Compile / scalaSource := baseDirectory.value / "src",
    name := "FireFlyv2",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
    libraryDependencies += "org.scalanlp" %% "breeze" % "1.0", // for numeric & matrix operations
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9", // for scala test
    libraryDependencies += djlCore,
    libraryDependencies += djlBackend,
    libraryDependencies += snakeYaml
  )

fork := true