package org.jetbrains.plugins.scala.codeInspection.modifiers

import com.intellij.codeInspection.{LocalInspectionTool, LocalQuickFixOnPsiElement, ProblemHighlightType, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.psi.search.{PsiSearchHelper, TextOccurenceProcessor, UsageSearchContext}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiElement, PsiElementVisitor, PsiFile}
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiModifierListOwnerExt}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReference
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScNewTemplateDefinition, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScModifierListOwner, ScNamedElement}

import scala.collection.mutable

private[modifiers] final class AccessModifierCanBeWeakerInspection extends LocalInspectionTool {
  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor =
    new PsiElementVisitor {
      override def visitElement(element: PsiElement): Unit = {
        (element, element) match {
          case (namedElement: ScNamedElement, modifierListOwner: ScModifierListOwner)
            if !modifierListOwner.hasModifierPropertyScala("private") =>
            namedElement match {
              case _: ScFunctionDefinition | _: ScTypeDefinition =>
                processElement(namedElement, modifierListOwner, holder)
              case _ =>
            }
          case (refPat: ScReferencePattern, _) =>
            val modifierListOwner = refPat.getParent.getParent.asInstanceOf[ScModifierListOwner]
            processElement(refPat, modifierListOwner, holder)
          case _ =>
        }
      }
    }

  private def processElement(
    element: ScNamedElement,
    modifierListOwner: ScModifierListOwner,
    problemsHolder: ProblemsHolder
  ): Unit = {
    val helper = PsiSearchHelper.getInstance(element.getProject)
    val usages = new mutable.ListBuffer[PsiElement]
    lazy val elementContainer = PsiTreeUtil.getParentOfType(element, classOf[PsiClass])
    lazy val elementContainerQualifiedName = if (elementContainer == null) "" else elementContainer.getQualifiedName

    val processor = new TextOccurenceProcessor {
      override def execute(e2: PsiElement, offsetInElement: Int): Boolean = {

        val e2Container = PsiTreeUtil.getParentOfType(e2, classOf[PsiClass])

        if (!e2Container.is[ScNewTemplateDefinition]) {
          lazy val e2ContainerQualifiedName = if (e2Container == null) "" else e2Container.getQualifiedName
          lazy val e2ContainerIsElementContainer = e2ContainerQualifiedName == elementContainerQualifiedName

          if (e2.is[ScReferenceExpression] || e2.is[ScStableCodeReference]) {
            if ((e2.getContainingFile != element.getContainingFile && e2ContainerIsElementContainer) ||
              elementContainer != e2Container) {
              usages.addOne(e2)
            }
          }
        }

        val continueExecution = true
        continueExecution
      }
    }

    helper.processElementsWithWord(
      processor,
      element.getUseScope,
      element.name,
      (UsageSearchContext.IN_CODE | UsageSearchContext.IN_FOREIGN_LANGUAGES).toShort,
      true
    )

    lazy val usagesThatRequirePublic = usages.filter { usage =>
      val e2Container = Option(PsiTreeUtil.getParentOfType(usage, classOf[PsiClass]))
      val e2IsInSubclassOfElement = elementContainer != null && e2Container.exists(_.isInheritor(elementContainer, true))
      !e2IsInSubclassOfElement
    }

    if (usages.isEmpty) {
      val fix = new MakePrivateQuickFix(modifierListOwner)
      problemsHolder.registerProblem(element.nameId, "Can be private", ProblemHighlightType.WARNING, fix)
    } else if (usagesThatRequirePublic.isEmpty) {
      val fix = new MakeProtectedQuickFix(modifierListOwner)
      problemsHolder.registerProblem(element.nameId, "Can be protected", ProblemHighlightType.WARNING, fix)
    }
  }
}

private class MakePrivateQuickFix(element: ScModifierListOwner) extends LocalQuickFixOnPsiElement(element) {

  override def invoke(project: Project, file: PsiFile, startElement: PsiElement, endElement: PsiElement): Unit =
    element.setModifierProperty("private")

  override def getText: String = "Make private"

  override def getFamilyName: String = "Change access modifier"
}

private class MakeProtectedQuickFix(element: ScModifierListOwner) extends LocalQuickFixOnPsiElement(element) {

  override def invoke(project: Project, file: PsiFile, startElement: PsiElement, endElement: PsiElement): Unit =
    element.setModifierProperty("protected")

  override def getText: String = "Make protected"

  override def getFamilyName: String = "Change access modifier"
}
