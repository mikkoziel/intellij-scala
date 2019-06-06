package org.jetbrains.plugins.scala.lang.actions.editor

import org.jetbrains.plugins.scala.base.EditorActionTestBase

class ClosingBraceInsertTest extends EditorActionTestBase {

  private def doTest(before: String, after: String): Unit = {
    checkGeneratedTextAfterTyping(before, after, '{')
  }

  def testInsert_ForStatement_Empty(): Unit = {
    val before = s"for (_ <- Seq()) $CARET"
    val after = s"for (_ <- Seq()) {$CARET}"
    doTest(before, after)
  }

  def testWrapInsert_ForStatement(): Unit = {
    val before =
      s"""for (_ <- Seq()) $CARET
         |  obj.method()
         |""".stripMargin
    val after =
      s"""for (_ <- Seq()) {
         |  ${CARET}obj.method()
         |}
         |""".stripMargin
    doTest(before, after)
  }

  def testInsert_FunctionBody_Empty(): Unit = {
    val before = s"def foo = $CARET"
    val after = s"def foo = {$CARET}"
    doTest(before, after)
  }

  def testWrapInsert_FunctionBody_Indented(): Unit = {
    val before =
      s"""def foo = $CARET
         |  obj.method()
         |    .method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {
         |  ${CARET}obj.method()
         |    .method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_FunctionBody_Indented_CaretRightAfterEqualsCharacter(): Unit = {
    val before =
      s"""def foo =$CARET
         |  obj.method()
         |    .method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {
         |  ${CARET}obj.method()
         |    .method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_FunctionBody_Indented_WithOneLineCommentBeforeBody(): Unit = {
    val before =
      s"""def foo = $CARET // comment line
         |  obj.method()
         |    .method
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = { // comment line
         |  ${CARET}obj.method()
         |    .method
         |}
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_FunctionBody_Indented_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""def foo = $CARET
         |  obj.method() // comment line
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {
         |  ${CARET}obj.method() // comment line
         |}
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_FunctionBody_Indented_WithBlockCommentBeforeBody(): Unit = {
    val before =
      s"""def foo = $CARET /* block comment */
         |  obj.method()
         |    .method
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = { /* block comment */
         |  ${CARET}obj.method()
         |    .method
         |}
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotWrapInsert_FunctionBody_NonIndented(): Unit = {
    val before =
      s"""def foo = $CARET
         |someUnrelatedCode1()
         |someUnrelatedCode2()
         |""".stripMargin
    val after =
      s"""def foo = {$CARET}
         |someUnrelatedCode1()
         |someUnrelatedCode2()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotWrapInsert_FunctionBody_NonIndented_1(): Unit = {
    val before =
      s"""class A {
         |  def foo = $CARET
         |  someUnrelatedCode1()
         |  someUnrelatedCode2()
         |}
         |""".stripMargin
    val after =
      s"""class A {
         |  def foo = {$CARET}
         |  someUnrelatedCode1()
         |  someUnrelatedCode2()
         |}
         |""".stripMargin
    doTest(before, after)
  }

  def testNotInsert_FunctionBody_NonIndented_UsingTabs(): Unit = {
    val indentOptions = getCommonSettings.getIndentOptions
    indentOptions.USE_TAB_CHARACTER = true
    indentOptions.TAB_SIZE = 4
    val before =
      s"""class A {
         |\t{
         |\t\tdef foo = $CARET
         |\t\tobj.methodCall
         |\t}
         |}""".stripMargin
    val after =
      s"""class A {
         |\t{
         |\t\tdef foo = {$CARET}
         |\t\tobj.methodCall
         |\t}
         |}""".stripMargin

    doTest(before, after)
  }

  def testNotInsert_FunctionBody_WithCaretAtBodyStart(): Unit = {
    val before =
      s"""def foo = ${CARET}obj.method()
         |  .method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {${CARET}obj.method()
         |  .method()
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotInsert_FunctionBody_WithCaretAtBodyStart_1(): Unit = {
    val before =
      s"""def foo =
         |  ${CARET}obj.method()
         |    .method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo =
         |  {${CARET}obj.method()
         |    .method()
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotWrapInsert_FunctionBodyWithCaretAtBodyStart_2(): Unit = {
    val before =
      s"""def foo = ${CARET}obj.method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {${CARET}obj.method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotInsert_FunctionBody_WithCaretAtBodyStart_3(): Unit = {
    val before =
      s"""def foo = $CARET   obj.method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""def foo = {$CARET   obj.method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_ValInitializer_Indented(): Unit = {
    val before =
      s"""val x = $CARET
         |  obj.method()
         |    .method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""val x = {
         |  ${CARET}obj.method()
         |    .method()
         |}
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_VarInitializer_Indented(): Unit = {
    val before =
      s"""var x = $CARET
         |  obj.method()
         |    .method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""var x = {
         |  ${CARET}obj.method()
         |    .method()
         |}
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_LazyValInitializer_Indented(): Unit = {
    val before =
      s"""lazy val x = $CARET
         |  obj.method()
         |    .method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""lazy val x = {
         |  ${CARET}obj.method()
         |    .method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotWrapInsert_ValInitializer_WithMultipleBindings(): Unit = {
    val before =
      s"""val (x, y) = $CARET
         |  (42, 23)
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""val (x, y) = {$CARET}
         |  (42, 23)
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotWrapInsert_ValInitializer_NonIndented(): Unit = {
    val before =
      s"""val x = $CARET
         |obj.method()
         |  .method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""val x = {$CARET}
         |obj.method()
         |  .method()
         |
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_If_ThenBranch(): Unit = {
    val before =
      s"""if (true) $CARET
         |  obj.method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_If_ThenBranch_1(): Unit = {
    val before =
      s"""if (true) $CARET
         |  obj.method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_If_ThenBranch_WithOneLineCommentBeforeBody(): Unit = {
    val before =
      s"""if (true) $CARET //comment
         |  obj.method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) { //comment
         |  ${CARET}obj.method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_If_ThenBranch_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""if (true) $CARET
         |  obj.method() //comment
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method() //comment
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }


  def testWrapInsert_If_ThenBranch_WithBlockCommentBeforeBody(): Unit = {
    val before =
      s"""if (true) $CARET /* block comment */
         |  obj.method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) { /* block comment */
         |  ${CARET}obj.method()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_IfElse_ThenBranch(): Unit = {
    val before =
      s"""if (true) $CARET
         |  obj.method()
         |else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method()
         |} else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_IfElse_ThenBranch_ForceElseOnNewLine(): Unit = {
    getCommonSettings.ELSE_ON_NEW_LINE = true
    val before =
      s"""if (true) $CARET
         |  obj.method()
         |else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method()
         |}
         |else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_IfElse_ThenBranch_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""if (true) $CARET
         |  obj.method() //comment
         |else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {
         |  ${CARET}obj.method() //comment
         |} else
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_IfElse_Else(): Unit = {
    val before =
      s"""if (true)
         |  obj.method()
         |else $CARET
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true)
         |  obj.method()
         |else {
         |  ${CARET}obj.method2()
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_IfElse_ElseBranch_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""if (true)
         |  obj.method()
         |else $CARET
         |  obj.method2() //comment
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true)
         |  obj.method()
         |else {
         |  ${CARET}obj.method2() //comment
         |}
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testNotInsert_IfElse_ThenBranch_SameLine(): Unit = {
    val before =
      s"""if (true) ${CARET}obj.method()
         |else
         |  obj.method2() //comment
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""if (true) {${CARET}obj.method()
         |else
         |  obj.method2() //comment
         |someUnrelatedCode()
         |""".stripMargin
    doTest(before, after)
  }

  def testWrapInsert_TryBlock(): Unit = {
    val before =
      s"""try $CARET
         |  obj.method()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""try {
         |  ${CARET}obj.method()
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_TryCatchBlock(): Unit = {
    val before =
      s"""try $CARET
         |  obj.method()
         |catch {
         |  case _ =>
         |}
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""try {
         |  ${CARET}obj.method()
         |} catch {
         |  case _ =>
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_TryCatchBlock_ForceCatchOnNewLine(): Unit = {
    getCommonSettings.CATCH_ON_NEW_LINE = true
    val before =
      s"""try $CARET
         |  obj.method()
         |catch {
         |  case _ =>
         |}
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""try {
         |  ${CARET}obj.method()
         |}
         |catch {
         |  case _ =>
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_FinallyBlock(): Unit = {
    val before =
      s"""try
         |  obj.method()
         |finally $CARET
         |  obj.method2()
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""try
         |  obj.method()
         |finally {
         |  ${CARET}obj.method2()
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_FinallyBlock_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""try
         |  obj.method()
         |finally $CARET
         |  obj.method2() //comment
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""try
         |  obj.method()
         |finally {
         |  ${CARET}obj.method2() //comment
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  // TODO: with finally block it is hard to implement this because it
  //  does not contain psi element which would denote finally block body
  //  def testWrapInsert_FinallyBlock_WithOneLineCommentBeforeBody(): Unit = {
  //    val before =
  //      s"""try
  //         |  obj.method()
  //         |finally $CARET //comment
  //         |  obj.method2()
  //         |someUnrelatedCode()
  //         |""".stripMargin
  //    val after =
  //      s"""try
  //         |  obj.method()
  //         |finally { //comment
  //         |  ${CARET}obj.method2()
  //         |}
  //         |someUnrelatedCode()
  //         |""".stripMargin
  //
  //    doTest(before, after)
  //  }

  def testWrapInsert_DoWhile(): Unit = {
    val before =
      s"""do $CARET
         |  42
         |while (true)
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""do {
         |  ${CARET}42
         |} while (true)
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_DoWhile_WithOneLineCommentBeforeBody(): Unit = {
    val before =
      s"""do $CARET//comment
         |  42
         |while (true)
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""do {//comment
         |  ${CARET}42
         |} while (true)
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_DoWhile_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""do $CARET
         |  42 //comment
         |while (true)
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""do {
         |  ${CARET}42 //comment
         |} while (true)
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_DoWhile_ForceWhileOnNewLine(): Unit = {
    getCommonSettings.WHILE_ON_NEW_LINE = true
    val before =
      s"""do $CARET
         |  42
         |while (true)
         |someUnrelatedCode()
         |""".stripMargin
    val after =
      s"""do {
         |  ${CARET}42
         |}
         |while (true)
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_While(): Unit = {
    val before =
      s"""while(false) $CARET
         |  42
         |someUnrelatedCode()
         |""".stripMargin

    val after =
      s"""while(false) {
         |  ${CARET}42
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_While_WithOneLineCommentBeforeBody(): Unit = {
    val before =
      s"""while(false) $CARET//comment
         |  42
         |someUnrelatedCode()
         |""".stripMargin

    val after =
      s"""while(false) {//comment
         |  ${CARET}42
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testWrapInsert_While_WithOneLineCommentAfterBody(): Unit = {
    val before =
      s"""while(false) $CARET
         |  42 //comment
         |someUnrelatedCode()
         |""".stripMargin

    val after =
      s"""while(false) {
         |  ${CARET}42 //comment
         |}
         |someUnrelatedCode()
         |""".stripMargin

    doTest(before, after)
  }

  def testNotInsert_IfBraceIsInsideString(): Unit = {
    val quotes = "\"\"\""
    val before =
      s"""val x =
         |  $quotes
         |  |$CARET
         |  $quotes
         |""".stripMargin

    val after =
      s"""val x =
         |  $quotes
         |  |{$CARET
         |  $quotes
         |""".stripMargin

    doTest(before, after)
  }

}
