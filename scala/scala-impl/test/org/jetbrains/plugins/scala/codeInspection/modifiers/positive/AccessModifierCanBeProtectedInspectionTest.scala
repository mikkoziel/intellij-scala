package org.jetbrains.plugins.scala.codeInspection.modifiers.positive

import org.jetbrains.plugins.scala.codeInspection.ScalaHighlightsTestBase
import org.jetbrains.plugins.scala.codeInspection.modifiers.AccessModifierCanBeWeakerInspection

class AccessModifierCanBeProtectedInspectionTest extends ScalaHighlightsTestBase {

  override protected val description = "Can be protected"

  override def setUp(): Unit = {
      super.setUp()
      myFixture.enableInspections(classOf[AccessModifierCanBeWeakerInspection])
  }

  def test_used_method(): Unit = {
    val code =
      s"""private class A { def ${START}foo$END = {} }
         |private class B extends A { foo }""".stripMargin
    checkTextHasError(code, allowAdditionalHighlights = false)
  }

  def test_used_val(): Unit = {
    val code =
      s"""private class A { val ${START}foo$END = 42 }
       |private class B extends A { foo }""".stripMargin
    checkTextHasError(code, allowAdditionalHighlights = false)
  }

  def test_used_var(): Unit = {
    val code =
      s"""private class A { var ${START}foo$END = 42 }
       |private class B extends A { foo }""".stripMargin
    checkTextHasError(code, allowAdditionalHighlights = false)
  }
}
