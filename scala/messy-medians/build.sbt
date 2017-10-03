name := "messy-medians"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.9.5" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
        