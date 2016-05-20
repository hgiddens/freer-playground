scalaVersion := "2.11.8"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-Xlint",
  "-Xlog-free-terms",
  "-Xmax-classfile-name", "124",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)
scalacOptions in Test += "-Yrangepos"

libraryDependencies ++= Seq(
  "org.atnos" %% "eff-cats" % "1.6.2"
)
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")