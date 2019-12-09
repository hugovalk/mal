ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.devdiscoveries"

lazy val scaladd = (project in file("."))
	.settings(
	  name := "ScalaDD",
		version := "0.1"
	)
