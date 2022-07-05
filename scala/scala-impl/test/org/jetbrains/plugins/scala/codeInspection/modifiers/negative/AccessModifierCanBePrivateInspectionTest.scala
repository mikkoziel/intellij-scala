package org.jetbrains.plugins.scala.codeInspection.modifiers.negative

final class AccessModifierCanBePrivateInspectionTest extends SharedTests {
  override protected val description = "Can be private"

  def test_used_class(): Unit = {
    val file = myFixture.addFileToProject("B.scala", "class B extends A")
    myFixture.openFileInEditor(file.getVirtualFile)
    val code = "class A"
    checkTextHasNoErrors(code)
  }
}
