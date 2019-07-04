lazy val root = (project in file("."))
  .settings(
    organization := "ch.becompany",
    name := "timesheet",
    scalaVersion := "2.13.0",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.typelevel" %% "mouse" % "0.22",
      "co.fs2" %% "fs2-core" % "1.1.0-M1",
      "co.fs2" %% "fs2-io" % "1.1.0-M1",
      "com.beachape" %% "enumeratum" % "1.5.13"
    )
  )
