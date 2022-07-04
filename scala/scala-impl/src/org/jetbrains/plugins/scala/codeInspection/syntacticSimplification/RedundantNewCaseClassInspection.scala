package org.jetbrains.plugins.scala.codeInspection.syntacticSimplification

import com.intellij.codeInspection.{ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.codeInspection.{AbstractInspection, ScalaInspectionBundle}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScPrimaryConstructor}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScNewTemplateDefinition, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScTypeAlias}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateParents
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

import scala.annotation.nowarn

/**
  * mattfowler
  * 5/7/2016
  */
@nowarn("msg=" + AbstractInspection.DeprecationText)
class RedundantNewCaseClassInspection extends AbstractInspection(ScalaInspectionBundle.message("display.name.redundant.new.on.case.class")) {

  override def actionFor(implicit holder: ProblemsHolder, isOnTheFly: Boolean): PartialFunction[PsiElement, Any] = {
    case newTemplate: ScNewTemplateDefinition if !newTemplate.extendsBlock.isAnonymousClass =>
      if (hasRedundantNew(newTemplate)) {
        holder.registerProblem(newTemplate.getFirstChild, ScalaBundle.message("new.on.case.class.instantiation.redundant"),
          ProblemHighlightType.LIKE_UNUSED_SYMBOL, new RemoveNewQuickFix(newTemplate))
      }
  }

  private def hasRedundantNew(newTemplate: ScNewTemplateDefinition): Boolean = {
    val constructor = getConstructorInvocationFromTemplate(newTemplate)
    def resolvedConstructor = resolveConstructor(constructor)

    isCreatingSameType(newTemplate) &&
      constructorCallHasArgumentList(constructor) &&
      constructor.exists(hasApplyDefinedOnCaseClass) &&
      isProblemlessPrimaryConstructorOfCaseClass(resolvedConstructor) &&
      !isTypeAlias(resolvedConstructor)
  }

  private def hasApplyDefinedOnCaseClass(constrInvocation: ScConstructorInvocation): Boolean = {
    val constructorText = constrInvocation.getText
    val expression = ScalaPsiElementFactory.createExpressionWithContextFromText(constructorText, constrInvocation.getContext, constrInvocation)
    val reference = getDeepestInvokedReference(expression).filter(_.isValid)

    reference.flatMap(_.bind())
      .exists {
        case ScalaResolveResult(f: ScFunctionDefinition, _) => f.syntheticNavigationElement.isInstanceOf[ScClass]
        case _ => false
      }
  }

  private def getDeepestInvokedReference(resolved: ScExpression): Option[ScReferenceExpression] = {
    resolved match {
      case method: ScMethodCall => method.deepestInvokedExpr match {
        case deepestRef: ScReferenceExpression => Some(deepestRef)
        case _ => None
      }
      case _ => None
    }
  }

  /**
    * Determines if the type of the extends block is the same as the type of the new template type.
    * This prevents us from incorrectly displaying a warning when creating anonymous classes or instances with
    * mixin traits.
    */
  private def isCreatingSameType(newTemplate: ScNewTemplateDefinition): Boolean =
    newTemplate.extendsBlock.templateParents.exists(_.parentClauses.size == 1)

  private def isTypeAlias(maybeResolveResult: Option[ScalaResolveResult]): Boolean = {
    maybeResolveResult.map(_.getActualElement).exists {
      case _: ScTypeAlias => true
      case _ => false
    }
  }

  private def getConstructorInvocationFromTemplate(newTemplate: ScNewTemplateDefinition): Option[ScConstructorInvocation] =
    newTemplate.extendsBlock.firstChild.flatMap {
      case parents: ScTemplateParents => parents.firstParentClause
      case _                          => None
    }

  private def constructorCallHasArgumentList(maybeConstructorInvocation: Option[ScConstructorInvocation]): Boolean = {
    maybeConstructorInvocation.flatMap(_.args).isDefined
  }

  private def resolveConstructor(maybeConstructorInvocation: Option[ScConstructorInvocation]): Option[ScalaResolveResult] = {
    for {
      constrInvoc <- maybeConstructorInvocation
      ref <- constrInvoc.reference
      resolved <- ref.bind()
    } yield {
      resolved
    }
  }

  private def isProblemlessPrimaryConstructorOfCaseClass(maybeResolveResult: Option[ScalaResolveResult]): Boolean = {
    maybeResolveResult
      .filter(_.problems.isEmpty)
      .map(_.element)
      .exists {
        case ScPrimaryConstructor.ofClass(clazz) => clazz.isCase
        case _ => false
      }
  }
}