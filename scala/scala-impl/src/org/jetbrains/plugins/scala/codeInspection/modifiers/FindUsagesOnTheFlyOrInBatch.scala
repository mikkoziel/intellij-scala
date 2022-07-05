package org.jetbrains.plugins.scala.codeInspection.modifiers

import com.intellij.codeInspection.deadCode.UnusedDeclarationInspectionBase
import com.intellij.openapi.roots.TestSourcesFilter
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.search.{LocalSearchScope, PsiSearchHelper, TextOccurenceProcessor, UsageSearchContext}
import com.intellij.psi.{PsiAnnotationOwner, PsiElement, PsiIdentifier, PsiNamedElement}
import org.jetbrains.plugins.scala.annotator.usageTracker.ScalaRefCountHolder
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, PsiFileExt, inReadAction}
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.{inNameContext, isOnlyVisibleInLocalFile, superValsSignatures}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReference
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSelfTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScEnumCase, ScFunction, ScFunctionDeclaration, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScEnum, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScModifierListOwner, ScNamedElement}
import org.jetbrains.plugins.scala.lang.psi.impl.search.ScalaOverridingMemberSearcher
import org.jetbrains.plugins.scala.project.ModuleExt
import org.jetbrains.plugins.scala.util.SAMUtil.PsiClassToSAMExt
import org.jetbrains.plugins.scala.util.{ScalaMainMethodUtil, ScalaUsageNamesUtil}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object FindUsagesOnTheFlyOrInBatch {

  sealed trait UsageType
  case object UsageRequiresPrivate extends UsageType
  case object UsageRequiresProtected extends UsageType
  case object UsageRequiresPublic extends UsageType

  private def getUsageTypesFor(element: ScNamedElement, isOnTheFly: Boolean): Seq[UsageType] =
    if (isOnlyVisibleInLocalFile(element)) {
      if (isOnTheFly) {
        val refCountHolder = ScalaRefCountHolder(element)

        var used = false
        val success = refCountHolder.runIfUnusedReferencesInfoIsAlreadyRetrievedOrSkip { () =>
          used = refCountHolder.isValueReadUsed(element) || refCountHolder.isValueWriteUsed(element)
        }

        Seq(if (!success || used) UsageRequiresPrivate else UsageRequiresPrivate)
      } else {
        referencesSearch(element)
      }
    } else referencesSearch(element) ++ checkIfEnumUsedOutsideScala(element) ++
      (element match {
        case f: ScFunctionDefinition if !f.name.endsWith("_=") =>
          ReferencesSearch.search(f).findAll().asScala.toSeq.map(_.getElement).map(_ => UsageRequiresPublic)
        case _ =>
          textSearch(element)
      })

  private def referencesSearch(element: ScNamedElement): Seq[UsageType] = {
    val elementsForSearch = element match {
      case enumCase: ScEnumCase =>
        val syntheticMembers =
          ScalaPsiUtil.getCompanionModule(enumCase.enumParent)
            .toSeq.flatMap(_.membersWithSynthetic)
            .collect {
              case n: ScNamedElement if ScalaUsageNamesUtil.enumSyntheticMethodNames.contains(n.name) => n
            }
        enumCase.getSyntheticCounterpart +: syntheticMembers
      case e: ScNamedElement => Seq(e)
    }

    val scope = new LocalSearchScope(element.getContainingFile)
    elementsForSearch.flatMap(ReferencesSearch.search(_, scope).findAll().asScala.toSeq).map(_ => UsageRequiresPublic)
  }

  private def checkIfEnumUsedOutsideScala(element: ScNamedElement): Seq[UsageType] = {
    val scEnum = element match {
      case el: ScEnumCase => Some(el.enumParent)
      case el: ScEnum => Some(el)
      case _ => None
    }

    scEnum.exists { e =>
      var used = false

      val processor = new TextOccurenceProcessor {
        override def execute(e2: PsiElement, offsetInElement: Int): Boolean =
          inReadAction {
            if (e2.getContainingFile.isScala3File || e2.getContainingFile.isScala2File) {
              true
            } else {
              used = true
              false
            }
          }
      }

      PsiSearchHelper
        .getInstance(element.getProject)
        .processElementsWithWord(
          processor,
          element.getUseScope,
          e.getName,
          (UsageSearchContext.IN_CODE | UsageSearchContext.IN_FOREIGN_LANGUAGES).toShort,
          true
        )

      if (!used) {
        PsiSearchHelper
          .getInstance(element.getProject)
          .processElementsWithWord(
            processor,
            element.getUseScope,
            s"${e.getName}$$.MODULE$$", // for usage of enum methods through `EnumName$.MODULE$.methodName(...)`
            (UsageSearchContext.IN_CODE | UsageSearchContext.IN_FOREIGN_LANGUAGES).toShort,
            true
          )
      }
      used
    }
    Seq(UsageRequiresPublic)
  }

  private def textSearch(element: ScNamedElement): Seq[UsageType] = {
    val helper = PsiSearchHelper.getInstance(element.getProject)
    var used = false
    val processor = new TextOccurenceProcessor {
      override def execute(e2: PsiElement, offsetInElement: Int): Boolean = {
        inReadAction {
          if (element.getContainingFile == e2.getContainingFile) {
            true
          } else {
            used = (e2, Option(e2.getParent)) match {
              case (_, Some(_: ScReferencePattern)) => false
              case (_, Some(_: ScTypeDefinition)) => false
              case (_: PsiIdentifier, _) => true
              case (l: LeafPsiElement, _) if l.isIdentifier => true
              case (_: ScStableCodeReference, _) => true
              case _ => false
            }
            !used
          }
        }
      }
    }

    ScalaUsageNamesUtil.getStringsToSearch(element).asScala.foreach { name =>
      if (!used) {
        helper.processElementsWithWord(
          processor,
          element.getUseScope,
          name,
          (UsageSearchContext.IN_CODE | UsageSearchContext.IN_FOREIGN_LANGUAGES).toShort,
          true
        )
      }
    }
    used

    Seq(UsageRequiresPublic)
  }

  def collectUsages(element: PsiElement, isOnTheFly: Boolean): Seq[UsageType] = {

    if (!shouldProcessElement(element)) {
      Seq.empty
    } else {

      case class InspectedElement(original: ScNamedElement, delegate: ScNamedElement)

      val elements: Seq[InspectedElement] = element match {
        case functionDeclaration: ScFunctionDeclaration
          if Option(functionDeclaration.getContainingClass).exists(_.isSAMable) =>
          Option(functionDeclaration.getContainingClass).toSeq
            .collect { case named: ScNamedElement => named }
            .map(InspectedElement(functionDeclaration, _))
        case named: ScNamedElement => Seq(InspectedElement(named, named))
        case _ => Seq.empty
      }
      elements.flatMap {
        case InspectedElement(_, _: ScTypeParam) if !isOnTheFly => Seq.empty
        case InspectedElement(_, typeParam: ScTypeParam) if typeParam.hasBounds || typeParam.hasImplicitBounds => Seq.empty
        case InspectedElement(_, inNameContext(holder: PsiAnnotationOwner)) if hasUnusedAnnotation(holder) =>
          Seq.empty
        case InspectedElement(original: ScNamedElement, delegate: ScNamedElement) =>
          getUsageTypesFor(delegate, isOnTheFly)
        case _ =>
          Seq.empty
      }
    }
  }

  def shouldProcessElement(elem: PsiElement): Boolean = elem match {
    case e if !isOnlyVisibleInLocalFile(e) && TestSourcesFilter.isTestSources(e.getContainingFile.getVirtualFile, e.getProject) => false
    case _: ScSelfTypeElement => false
    case e: ScalaPsiElement if e.module.exists(_.isBuildModule) => false
    case e: PsiElement if UnusedDeclarationInspectionBase.isDeclaredAsEntryPoint(e) && !ScalaPsiUtil.isImplicit(e) => false
    case obj: ScObject if ScalaMainMethodUtil.hasScala2MainMethod(obj) => false
    case n: ScNamedElement if n.nameId == null || n.name == "_" || isOverridingOrOverridden(n) => false
    case n: ScNamedElement =>
      n match {
        case p: ScModifierListOwner if hasOverrideModifier(p) => false
        case fd: ScFunctionDefinition if ScalaMainMethodUtil.isMainMethod(fd) => false
        case f: ScFunction if f.isSpecial || isOverridingFunction(f) => false
        case p: ScClassParameter if p.isCaseClassVal || p.isEnumVal || p.isEnumCaseVal => false
        case p: ScParameter =>
          p.parent.flatMap(_.parent.flatMap(_.parent)) match {
            case Some(_: ScFunctionDeclaration) => false
            case Some(f: ScFunctionDefinition) if ScalaOverridingMemberSearcher.search(f).nonEmpty ||
              isOverridingFunction(f) || ScalaMainMethodUtil.isMainMethod(f) => false
            case _ => true
          }
        case _ => true
      }
    case _ => false
  }

  private def hasOverrideModifier(member: ScModifierListOwner): Boolean =
    member.hasModifierPropertyScala(ScalaModifier.OVERRIDE)

  private def isOverridingOrOverridden(element: PsiNamedElement): Boolean =
    superValsSignatures(element, withSelfType = true).nonEmpty || isOverridden(element)

  private def isOverridingFunction(func: ScFunction): Boolean =
    hasOverrideModifier(func) || func.superSignatures.nonEmpty || isOverridden(func)

  private def isOverridden(member: PsiNamedElement): Boolean =
    ScalaOverridingMemberSearcher.search(member, deep = false, withSelfType = true).nonEmpty

  private def hasUnusedAnnotation(holder: PsiAnnotationOwner): Boolean =
    holder.hasAnnotation("scala.annotation.unused") ||
      // not entirely correct, but if we find @nowarn here in this situation
      // we can assume that it is directed at the unusedness of the symbol
      holder.hasAnnotation("scala.annotation.nowarn")
}
