name := "sparkTutorial"

version := "0.1"

scalaVersion := "2.11.12"
val sparkVer = "2.2.0"

libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-core" % sparkVer,
    "org.apache.spark" %% "spark-sql" % sparkVer,
    "org.apache.spark" %% "spark-graphx" % sparkVer,
    "org.apache.spark" %% "spark-mllib" % sparkVer,
    "org.apache.spark" %% "spark-streaming" % sparkVer
)