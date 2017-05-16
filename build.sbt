 scalaVersion := "2.12.2"
 name := "qidong"
 val scalazVersion = "7.3.0-M12"

//scala libs
libraryDependencies ++= Seq(
  "org.scalaz"            %% "scalaz-core"       % scalazVersion,
  "org.scalaz"            %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz"            %% "scalaz-effect"     % scalazVersion,
  "io.monix" 		  %% "monix"             % "2.3.0")

//java libs
libraryDependencies += "joda-time" % "joda-time" % "2.9.9"

//test libs
libraryDependencies ++= Seq(
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test")

autoCompilerPlugins := true
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalacOptions ++= Seq(
//  "-P:continuations:enable",
//  "-Yliteral-types",
//  "-Yinduction-heuristics",
  "-Ypartial-unification",
//  "-Ykind-polymorphism",
  "-target:jvm-1.8",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions")

