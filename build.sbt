name := "foodmap"

scalaVersion := "2.11.8"

resolvers ++= Seq(
)

{
  libraryDependencies ++= Seq(
    "org.encog" % "encog-core" % "3.3.0",
    "org.scala-lang" % "scala-swing" % "2.11+"
  )
}



scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Yinline", "-Yinline-warnings",
  "-language:_",
  // "-Xdisable-assertions",
  "-optimize"
)

