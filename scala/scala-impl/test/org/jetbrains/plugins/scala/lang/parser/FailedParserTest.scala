package org.jetbrains.plugins.scala
package lang
package parser

import org.jetbrains.plugins.scala.base.ScalaFileSetTestCase
import org.junit.runner.RunWith
import org.junit.runners.AllTests

@RunWith(classOf[AllTests])
class FailedParserTest extends ScalaFileSetTestCase("/parser/failed") {

  override protected def shouldPass = false
}

object FailedParserTest {
  def suite = new FailedParserTest()
}