name := """Natural"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.6"


// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

initialCommands in console := """
import com.github.buster84._
type _0 = 零
type _1 = 後者[_0]
type _2 = 後者[_1]
type _3 = 後者[_2]
type _4 = 後者[_3]
type _5 = 後者[_4]
type _6 = 後者[_5]
type _7 = 後者[_6]
type _8 = 後者[_7]
type _9 = 後者[_8]
"""
