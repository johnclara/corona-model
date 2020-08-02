
scalaVersion := "2.13.1"

name := "corona-model"
organization := "jclara"
version := "1.0"



lazy val specs2Version = "4.10.1"
def specs2(scope: String) =
  Seq(
    "org.specs2" %% "specs2-core" % specs2Version,
    "org.specs2" %% "specs2-mock" % specs2Version
  ).map(lib => lib % scope)
libraryDependencies ++= specs2("test")
