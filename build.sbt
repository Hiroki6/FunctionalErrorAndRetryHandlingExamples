name := "ErrorRetryHandling"

version := "0.1"

scalaVersion := "2.13.1"

val catsRetryVersion = "0.3.2"

libraryDependencies ++= Seq(
  "com.github.cb372" %% "cats-retry-core"        % catsRetryVersion,
  "com.github.cb372" %% "cats-retry-cats-effect" % catsRetryVersion
)
