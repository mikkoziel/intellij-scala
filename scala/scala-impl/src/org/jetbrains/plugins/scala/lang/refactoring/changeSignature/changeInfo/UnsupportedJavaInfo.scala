package org.jetbrains.plugins.scala
package lang.refactoring.changeSignature.changeInfo
import com.intellij.psi.PsiMethod
import com.intellij.refactoring.changeSignature.ThrownExceptionInfo

import java.util

/**
 * Nikolay.Tropin
 * 2014-08-28
 */
private[changeInfo] trait UnsupportedJavaInfo {

  def getNewExceptions: Array[ThrownExceptionInfo] = Array()

  def isExceptionSetChanged: Boolean = false

  def isExceptionSetOrOrderChanged: Boolean = false

  def getMethodsToPropagateParameters: util.Collection[PsiMethod] = util.Collections.emptyList()
}
