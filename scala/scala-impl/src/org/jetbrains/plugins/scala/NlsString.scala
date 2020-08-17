package org.jetbrains.plugins.scala

import org.jetbrains.annotations.Nls

/**
 * Marks a string as containing a natural language string
 *
 * Use when @Nls isn't possible... like in tuples
 *
 * @param nls the content
 */
case class NlsString(@Nls nls: String) extends AnyVal

object NlsString {
  // this makes the creation of an NlsString @Nls
  @Nls
  def apply(@Nls nls: String): NlsString = new NlsString(nls)

  /**
   * Use to force a string to become an NlsString
   */
  @Nls
  def force(nls: String): NlsString = {
    //noinspection ReferencePassedToNls
    new NlsString(nls)
  }

  /**
   * Implicit conversion to a normal string
   */
  @Nls
  implicit def toNormalString(nlsString: NlsString): String =
    nlsString.nls
}