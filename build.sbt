name := "mercury"

organization := "com.gu"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
    "org.joda" % "joda-convert" % "1.7" % "provided",
    "joda-time" % "joda-time" % "2.7",
    "net.databinder" %% "unfiltered-filter" % "0.8.4",
    "io.spray" %%  "spray-json" % "1.3.1",
    "org.slf4j" % "slf4j-api" % "1.7.10",
    "org.slf4j" % "slf4j-jdk14" % "1.7.10",
    "com.google.appengine" % "appengine-api-1.0-sdk" % "1.9.17",
    "org.jsoup" % "jsoup" % "1.8.1",
    "javax.servlet" % "servlet-api" % "2.5" % "provided",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "7.6.16.v20140903" % "container"

scalacOptions ++= Seq("-feature", "-deprecation")

appengineSettings

SbtTwirl.projectSettings