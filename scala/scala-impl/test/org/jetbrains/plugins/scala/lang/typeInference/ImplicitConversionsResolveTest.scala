package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

class ImplicitConversionsResolveTest extends ScalaLightCodeInsightFixtureTestAdapter {
  def testSCL17570(): Unit = checkTextHasNoErrors(
    s"""
       |object A {
       | val l: Long = 1
       | val l2: java.lang.Long = 1
       |}
       |""".stripMargin
  )

  def testSCL15323(): Unit = checkTextHasNoErrors(
    """
      |object SelfTypeTests {
      |  trait Foo {
      |    def foo(): Int = 42
      |  }
      |
      |  object Foo {
      |    implicit class Ext(private val f: Foo) extends AnyVal {
      |      def fooExt(): Int = 23
      |    }
      |  }
      |
      |  trait Bar { self: Foo =>
      |    def bar(): Int = {
      |      self.foo()
      |      self.fooExt()
      |    }
      |  }
      |}
      |""".stripMargin
  )
}
