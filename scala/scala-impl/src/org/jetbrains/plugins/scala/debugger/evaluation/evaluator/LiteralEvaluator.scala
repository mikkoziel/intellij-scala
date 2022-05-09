package org.jetbrains.plugins.scala.debugger.evaluation
package evaluator

import com.intellij.debugger.JavaDebuggerBundle
import com.intellij.debugger.engine.DebuggerUtils
import com.intellij.debugger.engine.evaluation.EvaluationContextImpl
import com.intellij.debugger.engine.evaluation.expression.Evaluator
import com.intellij.debugger.impl.DebuggerUtilsEx
import com.intellij.openapi.util.registry.Registry
import com.sun.jdi._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.types.ScLiteralType

import java.util.Collections

private[evaluation] final class LiteralEvaluator private (value: AnyRef, expectedType: String) extends Evaluator {
  override def evaluate(context: EvaluationContextImpl): Value = {
    val vm = context.getDebugProcess.getVirtualMachineProxy

    value match {
      case null => null
      case b: java.lang.Boolean => DebuggerUtilsEx.createValue(vm, expectedType, b.booleanValue())
      case c: java.lang.Character => DebuggerUtilsEx.createValue(vm, expectedType, c.charValue())
      case d: java.lang.Double => DebuggerUtilsEx.createValue(vm, expectedType, d.doubleValue())
      case f: java.lang.Float => DebuggerUtilsEx.createValue(vm, expectedType, f.floatValue())
      case n: java.lang.Number => DebuggerUtilsEx.createValue(vm, expectedType, n.longValue())
      case s: String =>
        vm.mirrorOfStringLiteral(s, () => {
          val ref = DebuggerUtilsEx.mirrorOfString(s, vm, context)
          if (Registry.is("debugger.intern.string.literals") && vm.versionHigher("1.7")) {
            val internMethod = DebuggerUtils.findMethod(ref.referenceType(), "intern", "()Ljava/lang/String;")
            if (internMethod ne null) {
              context.getDebugProcess.invokeMethod(context, ref, internMethod, Collections.emptyList()).asInstanceOf[StringReference]
            } else ref
          } else ref
        })
      case _ =>
        throw EvaluationException(JavaDebuggerBundle.message("evaluation.error.unknown.expression.type", expectedType))
    }
  }
}

private[evaluation] object LiteralEvaluator {
  def fromLiteral(lit: ScLiteral): LiteralEvaluator = {
    val tpe = lit.`type`().getOrAny match {
      case lt: ScLiteralType => lt.wideType
      case t => t
    }

    val stdTypes = tpe.projectContext.stdTypes
    import stdTypes._

    val expected = tpe match {
      case Boolean => "boolean"
      case Byte => "byte"
      case Char => "char"
      case Double => "double"
      case Float => "float"
      case Int => "int"
      case Long => "long"
      case Short => "short"
      case _ => ""
    }

    new LiteralEvaluator(lit.getValue, expected)
  }
}