package org.jetbrains.plugins.scala.codeInspection.modifiers.negative

import org.jetbrains.plugins.scala.codeInspection.ScalaHighlightsTestBase
import org.jetbrains.plugins.scala.codeInspection.modifiers.AccessModifierCanBeWeakerInspection

abstract class SharedTests extends ScalaHighlightsTestBase {

  override def setUp(): Unit = {
    super.setUp()
    myFixture.enableInspections(classOf[AccessModifierCanBeWeakerInspection])
  }

  def test_used_val(): Unit = {
    val code = "private class A { val foo = 42 }; private class B { new A().foo }"
    checkTextHasNoErrors(code)
  }

  def test_used_var(): Unit = {
    val code = "private class A { var foo = 42 }; private class B { new A().foo }"
    checkTextHasNoErrors(code)
  }

  def test_used_method(): Unit = {
    val code = "private class A { def foo = {} }; private class B { new A().foo }"
    checkTextHasNoErrors(code)
  }
}
