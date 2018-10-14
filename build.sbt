scalaVersion := "2.12.6"
enablePlugins(ScalaJSPlugin)

//scalaJSUseMainModuleInitializer := true
libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6"
)
