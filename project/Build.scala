import sbt._
import Keys._

object Lab1Build extends Build {
  lazy val root = Project(id = "lab1",
                          base = file("."))

  lazy val grader = Project(id = "lab1-grader",
                            base = file("grader")) dependsOn(root)
}
