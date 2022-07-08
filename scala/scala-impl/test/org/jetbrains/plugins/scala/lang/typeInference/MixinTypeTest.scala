package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

class MixinTypeTest extends ScalaLightCodeInsightFixtureTestAdapter {

  def testSCL6573(): Unit = {
    val text =
      """
        |class SCL6573 {
        |  def foo = {
        |    trait A
        |
        |    trait B
        |
        |    new A with B
        |  }
        |}
      """.stripMargin
    checkTextHasNoErrors(text)
  }
}
