lazy val root = (project in file(".")).
	settings(
		version := "0.0.9",
		scalaVersion := "2.12.7",
		libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
		assemblyJarName in assembly := "objn.jar",
		TaskKey[Unit]("check") := {
			val process = sys.process.Process("java", Seq("-jar", (crossTarget.value / "objn.jar").toString))
			val out = (process!!)
			()
		}
	)
