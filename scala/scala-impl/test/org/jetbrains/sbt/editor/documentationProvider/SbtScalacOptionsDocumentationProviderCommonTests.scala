package org.jetbrains.sbt.editor.documentationProvider

import com.intellij.lang.documentation.DocumentationMarkup
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel
import org.jetbrains.sbt.language.psi.SbtScalacOptionDocHolder
import org.jetbrains.sbt.language.utils.SbtScalacOptionInfo
import org.jetbrains.sbt.language.utils.SbtScalacOptionInfo.ArgType

trait SbtScalacOptionsDocumentationProviderCommonTests {
  self: SbtScalacOptionsDocumentationProviderTestBase =>

  private val NONEXISTENT_FLAG = "-flag-that-no-one-should-ever-add-to-compiler"
  private val DEPRECATION_FLAG = "-deprecation"

  private lazy val DEPRECATION_DESCRIPTION = {
    import ScalaLanguageLevel._
    val langLevel = getVersion.languageLevel

    val description = langLevel match {
      case Scala_2_11 | Scala_3_0 =>
        "Emit warning and location for usages of deprecated APIs."
      case Scala_2_12 | Scala_2_13 =>
        "Emit warning and location for usages of deprecated APIs. See also -Wconf. [false]"
      case _ => throw new IllegalStateException(s"Unexpected language level: ${langLevel.getVersion}")
    }

    DocumentationMarkup.CONTENT_START +
      langLevel.getVersion + "<br>" +
      description +
      DocumentationMarkup.CONTENT_END
  }

  private def getVersion(implicit ev: ScalaVersion): ScalaVersion = ev

  def test_topLevel_single(): Unit = doGenerateDocTest(
    s"""scalacOptions += "${|}$DEPRECATION_FLAG"""",
    DEPRECATION_DESCRIPTION
  )

  def test_topLevel_seq(): Unit = doGenerateDocTest(
    s"""scalacOptions ++= Seq("${|}$DEPRECATION_FLAG")""",
    DEPRECATION_DESCRIPTION
  )

  def test_topLevel_complexExpression_seq(): Unit = doGenerateDocTest(
    s"""scalacOptions ++= {
       |  if (1 == 2) {
       |    Nil
       |  } else Seq("${|}$DEPRECATION_FLAG")
       |}""".stripMargin,
    DEPRECATION_DESCRIPTION
  )

  def test_inProjectSettings_single(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    scalacOptions += "${|}$DEPRECATION_FLAG"
       |  )
       |""".stripMargin,
    DEPRECATION_DESCRIPTION
  )

  def test_inProjectSettings_seq(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    scalacOptions ++= Seq("${|}$DEPRECATION_FLAG")
       |  )
       |""".stripMargin,
    DEPRECATION_DESCRIPTION
  )

  def test_inProjectSettings_complexExpression_seq(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    scalacOptions ++= {
       |      if (1 == 2) {
       |        Nil
       |      } else Seq("${|}$DEPRECATION_FLAG")
       |    }
       |  )
       |""".stripMargin,
    DEPRECATION_DESCRIPTION
  )

  def test_topLevel_single_notFound(): Unit = doGenerateDocTest(
    s"""scalacOptions += "${|}$NONEXISTENT_FLAG"""",
    null
  )

  def test_topLevel_seq_notFound(): Unit = doGenerateDocTest(
    s"""scalacOptions ++= Seq("${|}$NONEXISTENT_FLAG")""",
    null
  )

  def test_inProjectSettings_single_notFound(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    scalacOptions += "${|}$NONEXISTENT_FLAG"
       |  )
       |""".stripMargin,
    null
  )

  def test_inProjectSettings_seq_notFound(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    scalacOptions ++= Seq("${|}$NONEXISTENT_FLAG")
       |  )
       |""".stripMargin,
    null
  )

  def test_topLevel_single_notScalacOptions_notFound(): Unit = doGenerateDocTest(
    s"""javacOptions += "${|}$DEPRECATION_FLAG"""",
    null
  )

  def test_topLevel_seq_notScalacOptions_notFound(): Unit = doGenerateDocTest(
    s"""javacOptions ++= Seq("${|}$DEPRECATION_FLAG")""",
    null
  )

  def test_inProjectSettings_single_notScalacOptions_notFound(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    javacOptions += "${|}$DEPRECATION_FLAG"
       |  )
       |""".stripMargin,
    null
  )

  def test_inProjectSettings_seq_notScalacOptions_notFound(): Unit = doGenerateDocTest(
    s"""
       |lazy val foo = project.in(file("foo"))
       |  .settings(
       |    name := "foo",
       |    scalaVersion := "$getVersion",
       |    javacOptions ++= Seq("${|}$DEPRECATION_FLAG")
       |  )
       |""".stripMargin,
    null
  )

  def test_lookupElement(): Unit = {
    val langLevel = version.languageLevel
    val description = "Scalac options lookup element documentation test description"
    val descriptions = Map(description -> Set(langLevel))
    val option = SbtScalacOptionInfo("-test-flag", descriptions, Map.empty, ArgType.No, Set(langLevel), None)
    val docHolder = SbtScalacOptionDocHolder(option)(self.getFixture.getProject)

    val expectedDoc = DocumentationMarkup.CONTENT_START + langLevel.getVersion + "<br>" + description + DocumentationMarkup.CONTENT_END
    val actualDoc = generateDoc(docHolder, null)
    assertDocHtml(expectedDoc, actualDoc)
  }

}
