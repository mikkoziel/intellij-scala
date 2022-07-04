package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}


class ScalaTestInScala3HighlightingTest extends ScalaLightCodeInsightFixtureTestAdapter {
  override protected def supportedIn(version: ScalaVersion): Boolean =
    version  == LatestScalaVersions.Scala_3

  override def librariesLoaders: Seq[LibraryLoader] = {
    super.librariesLoaders :+ IvyManagedLoader("org.scalatest" %% "scalatest" % "3.2.12")
  }

  def testSCL20155(): Unit = checkTextHasNoErrors(
    """
      |import org.scalatest.wordspec.AnyWordSpec
      |
      |class WordSpecTest extends AnyWordSpec {
      |  "parent" which {
      |    "child" ignore { () }
      |    "child" in { () }
      |    "child" is { ??? }
      |    "child" that { () }
      |    "child" when { () }
      |    "child" which { () }
      |
      |    "child" can { () }
      |    "child" must { () }
      |    "child" should { () }
      |  }
      |}
      |""".stripMargin
  )

  def testSCL20155_2(): Unit = checkTextHasNoErrors(
    """
      |import org.scalatest.wordspec.*
      |
      |class WordSpecViewTest extends AnyWordSpec {
      |  "parent1" should {
      |    "pending1" in pending
      |    "pending2" is pending
      |    "pending2".is(pending)
      |  }
      |}
      |""".stripMargin
  )
}
