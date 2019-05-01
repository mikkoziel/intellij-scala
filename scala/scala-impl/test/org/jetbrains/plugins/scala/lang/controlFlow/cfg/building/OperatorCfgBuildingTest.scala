package org.jetbrains.plugins.scala.lang.controlFlow.cfg.building

class OperatorCfgBuildingTest extends CfgBuildingTestBase {
  def test_basic_infix(): Unit = {
    check(
      """
        |val a = 0 + 1
      """.stripMargin,
      """
        |a = call [0](1) +
        |end
      """.stripMargin
    )
  }
  def test_basic_prefix(): Unit = {
    check(
      """
        |val b = !true
      """.stripMargin,
      """
        |b = call [true]() unary_!
        |end
      """.stripMargin
    )
  }
  def test_basic_postfix(): Unit = {
    check(
      """
        |val c = (1 toString)
      """.stripMargin,
      """
        |c = call [1]() java.lang.Object.toString
        |end
      """.stripMargin
    )
  }

  def test_normal_name(): Unit = {
    check(
      """
        |val a = 1
        |val b = 2
        |a compare b
      """.stripMargin,
      """
        |a = 1
        |b = 2
        |%0 <- a
        |%1 <- b
        |call [%0](%1) scala.runtime.OrderedProxy.compare
        |end
      """.stripMargin
    )
  }

  def test_changed_associativity(): Unit = {
    check(
      """
        |object Test {
        |  def -:(value: Int): Unit = ()
        |}
        |val a = 9
        |a -: Test
      """.stripMargin,
      """
        |a = 9
        |%0 <- a
        |%1 <- Test$
        |call [%1](%0) Test$.$minus$colon
        |end
      """.stripMargin
    )
  }

  def test_changed_associativity_with_defaults(): Unit = {
    check(
      """
        |
        |object Test {
        |  def -:(value: Int, default: String = "default"): Unit = ()
        |}
        |val a = 9
        |a -: Test
      """.stripMargin,
      """
        |a = 9
        |%0 <- a
        |%1 <- Test$
        |call [%1](%0, "default") Test$.$minus$colon
        |end
      """.stripMargin
    )
  }
}
