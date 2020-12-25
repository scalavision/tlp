lazy val root = project
  .in(file("."))
  .settings(
    name := "tlp",
    version := "0.1.0",
    scalaVersion := "3.0.0-M3",
    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-Yindent-colons"
    ),
    mainClass := Some("tlp.Main"),
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.typelevel" %% "cats-effect" % "2.3.1",
      ("com.lihaoyi" %% "pprint" % "0.5.6").withDottyCompat(scalaVersion.value),
      ("io.getquill" %% "quill-core-portable" % "3.5.2").withDottyCompat(scalaVersion.value),
      ("io.getquill" %% "quill-sql-portable" % "3.5.2").withDottyCompat(scalaVersion.value),
//      "com.github.rssh" %% "dotty-cps-async" % "0.3.5"
    )
  )
