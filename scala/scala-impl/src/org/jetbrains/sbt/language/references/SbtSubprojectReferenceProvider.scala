package org.jetbrains.sbt
package language.references

import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.io.FileUtil
import com.intellij.psi._
import com.intellij.psi.search.{FilenameIndex, GlobalSearchScope}
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScInfixExpr, ScMethodCall, ScNewTemplateDefinition, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateParents

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
 * @author Nikolay Obedin
 * @since 8/26/14.
 */
class SbtSubprojectReferenceProvider extends PsiReferenceProvider {

  override def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] = {
    if (element.getContainingFile.getFileType.getName != Sbt.Name) return Array.empty
    extractSubprojectPath(element).flatMap { path =>
      findBuildFile(path, element.getProject).map(new SbtSubprojectReference(element, _))
    }.toArray
  }

  private def findBuildFile(subprojectPath: String, project: Project): Option[PsiFile] = {
    val vFiles = FilenameIndex.getVirtualFilesByName("build.sbt", GlobalSearchScope.allScope(project)).asScala
    val buildVFile = vFiles.find { vFile =>
      val relativeToProjectPath = project.getBasePath + File.separator + subprojectPath
      val absolutePath = FileUtil.toSystemIndependentName(FileUtil.toCanonicalPath(relativeToProjectPath))
      Option(vFile.getParent).map(_.getPath).fold(false)(FileUtil.comparePaths(_, absolutePath) == 0)
    }
    buildVFile.safeMap(PsiManager.getInstance(project).findFile)
  }

  private def extractSubprojectPath(element: PsiElement): Option[String] = {
    Option(element.getParent).safeMap(_.getParent) match {
      case Some(ScPatternDefinition.expr(e)) => e match {
        case expr: ScReferenceExpression if expr.textMatches("project") => Some(element.getText)
        case call: ScMethodCall => extractSubprojectPathFromProjectCall(call, element)
        case _ => None
      }
      case _ => None
    }
  }

  private def extractSubprojectPathFromProjectCall(call: ScMethodCall, element: PsiElement) = {
    var result: Option[String] = None
    val visitor = new ScalaRecursiveElementVisitor {
      override def visitMethodCallExpression(call: ScMethodCall): Unit = call match {
        case ScMethodCall(expr, Seq(_: ScLiteral, pathElt)) if expr.textMatches("Project") =>
          result = extractPathFromFileParam(pathElt)
        case ScMethodCall(expr, Seq(pathElt)) if expr.getText.matches("^project.+?in$") =>
          result = extractPathFromFileParam(pathElt)
        case ScMethodCall(expr, _) if expr.getText.startsWith("project") =>
          result = Some(element.getText)
          super.visitMethodCallExpression(call)
        case _ =>
          super.visitMethodCallExpression(call)
      }
    }
    call.accept(visitor)
    result
  }

  // TODO: extract these methods into another class and use them to write path completion

  private def extractPathFromFileParam(element: PsiElement): Option[String] = element match {
    case newFileDef : ScNewTemplateDefinition =>
      newFileDef.extendsBlock.getChildren.toSeq.headOption.collect {
        case classParent: ScTemplateParents => classParent.constructorInvocation.flatMap(extractPathFromFileCtor)
      }.flatten
    case expr@ScInfixExpr(_, op, _) if op.textMatches("/") =>
      extractPathFromConcatenation(expr)
    case expr : ScReferenceExpression =>
      Option(expr.resolve()).flatMap(extractPathFromReference)
    case ScMethodCall(expr, ScStringLiteral(path) :: _) if expr.textMatches("file") =>
      Option(path)
    case _ => None
  }

  private def extractPathFromFileCtor(constrInvocation: ScConstructorInvocation): Option[String] = {
    constrInvocation.args.map(_.exprs).flatMap {
      case Seq(ScStringLiteral(path)) =>
        Some(path)
      case Seq(ScStringLiteral(parent), ScStringLiteral(child)) =>
        Some(parent + File.separator + child)
      case Seq(parentElt, ScStringLiteral(child)) =>
        extractPathFromFileParam(parentElt).map(_ + File.separator + child)
      case _ => None
    }
  }

  private def extractPathFromConcatenation(concatExpr: ScInfixExpr): Option[String] =
    concatExpr.right match {
      case ScStringLiteral(child) =>
        extractPathFromFileParam(concatExpr.left).map(_ + File.separator + child)
      case partRef : ScReferenceExpression =>
        for {
          parent <- extractPathFromFileParam(concatExpr.left)
          child  <- extractPathFromFileParam(partRef)
        } yield parent + File.separator + child
      case _ => None
    }

  private def extractPathFromReference(ref: PsiElement): Option[String] = {
    for {
      listOfPatterns <- Option(ref.getParent)
      patternDef <- Option(listOfPatterns.getParent)
    } yield patternDef match {
      case ScPatternDefinition.expr(e) => extractPathFromFileParam(e)
      case _ => None
    }
  }.flatten
}

private class SbtSubprojectReference[T <: PsiElement](val element: T, val sbtFile: PsiFile)
        extends PsiReferenceBase.Immediate[T](element,
          TextRange.create(element.getStartOffsetInParent, element.getStartOffsetInParent + element.getTextLength),
          sbtFile)
