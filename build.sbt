scalaVersion := "2.12.6"
enablePlugins(ScalaJSPlugin)

//scalaJSUseMainModuleInitializer := true
libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  "io.circe" %%% "circe-generic" % "0.10.1",
  "io.circe" %%% "circe-parser" % "0.10.1"
)
