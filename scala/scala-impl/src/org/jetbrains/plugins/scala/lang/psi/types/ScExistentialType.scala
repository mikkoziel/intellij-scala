package org.jetbrains.plugins.scala
package lang
package psi
package types

import gnu.trove.{THashMap, TObjectHashingStrategy}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScTypeParam, TypeParamId}
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue._
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.util.HashBuilder._

import scala.annotation.tailrec

/**
  * @author ilyas
  */
final class ScExistentialType private (
  val quantified:         ScType,
  val wildcards:          List[ScExistentialArgument],
  private val simplified: Option[ScType]
) extends ScalaType
    with ValueType {

  override implicit def projectContext: project.ProjectContext = quantified.projectContext

  override protected def calculateAliasType: Option[AliasType] = {
    quantified.aliasType.map(a => a.copy(lower = a.lower.map(_.unpackedType), upper = a.upper.map(_.unpackedType)))
  }

  override def equivInner(r: ScType, constraints: ConstraintSystem, falseUndef: Boolean): ConstraintsResult = {
    if (r.equiv(Nothing)) return quantified.equiv(Nothing, constraints)
    val simplified = simplify()
    if (this != simplified) return simplified.equiv(r, constraints, falseUndef)
    (quantified, r) match {
      case (ParameterizedType(ScAbstractType(typeParameter, lowerBound, upperBound), args), _) if !falseUndef =>
        val subst = ScSubstitutor.bind(typeParameter.typeParameters, args)
        val upper: ScType =
          subst(upperBound) match {
            case ParameterizedType(u, _) => ScExistentialType(ScParameterizedType(u, args))
            case u => ScExistentialType(ScParameterizedType(u, args))
          }
        val conformance = r.conforms(upper, constraints)
        if (conformance.isLeft) return conformance

        val lower: ScType =
          subst(lowerBound) match {
            case ParameterizedType(l, _) => ScExistentialType(ScParameterizedType(l, args))
            case l => ScExistentialType(ScParameterizedType(l, args))
          }
        return lower.conforms(r, conformance.constraints)
      case (ParameterizedType(UndefinedType(typeParameter, _), args), _) if !falseUndef =>
        r match {
          case ParameterizedType(des, _) =>
            val y = ScalaConformance.addParam(typeParameter, des, constraints)
            if (y.isLeft) return ConstraintsResult.Left

            return ScExistentialType(ScParameterizedType(des, args)).equiv(r, y.constraints, falseUndef)
          case ScExistentialType(ParameterizedType(des, _), _) =>
            val y = ScalaConformance.addParam(typeParameter, des, constraints)
            if (y.isLeft) return ConstraintsResult.Left

            return ScExistentialType(ScParameterizedType(des, args)).equiv(r, y.constraints, falseUndef)
          case _ => return ConstraintsResult.Left //looks like something is wrong
        }
      case (ParameterizedType(pType, args), ParameterizedType(rType, _)) =>
        val res = pType.equivInner(rType, constraints, falseUndef)
        if (res.isLeft) return res
        val iter = extractTypeParameters(rType)
        val (names, existArgsBounds) =
          args.zip(iter.toList).collect {
            case (arg: ScExistentialArgument, rParam: ScTypeParam)
              if rParam.isCovariant && wildcards.contains(arg) => (arg.name, arg.upper)
            case (arg: ScExistentialArgument, rParam: ScTypeParam)
              if rParam.isContravariant && wildcards.contains(arg) => (arg.name, arg.lower)
          }.unzip
        val subst = ScSubstitutor.bind(names, existArgsBounds)(TypeParamId.nameBased)
        return subst(quantified).equiv(r, constraints, falseUndef)
      case _ =>
    }
    r.unpackedType match {
      case ex: ScExistentialType =>
        val simplified = ex.simplify()
        if (ex != simplified) return this.equiv(simplified, constraints, falseUndef)
        ScExistentialType.equivImpl(this, ex, constraints, falseUndef)
      case poly: ScTypePolymorphicType if poly.typeParameters.length == wildcards.length =>
        val list = wildcards.zip(poly.typeParameters)
        val iterator = list.iterator
        var t: ConstraintsResult = constraints

        while (iterator.hasNext) {
          val (w, tp) = iterator.next()

          t = w.lower.equivInner(tp.lowerType, t.constraints, falseUndef)
          if (t.isLeft) return ConstraintsResult.Left

          t = w.upper.equivInner(tp.upperType, t.constraints, falseUndef)
          if (t.isLeft) return ConstraintsResult.Left
        }
        val polySubst = ScSubstitutor.bind(poly.typeParameters, wildcards)
        quantified.equiv(polySubst(poly.internalType), t.constraints, falseUndef)
      case _ => ConstraintsResult.Left
    }
  }

  def simplify(): ScType = simplified.getOrElse(this)

  override def visitType(visitor: ScalaTypeVisitor): Unit = visitor.visitExistentialType(this)

  override def typeDepth: Int = quantified.typeDepth + 1

  //existential type is fully defined by `quantified`, other fields are computed from it
  override def equals(other: Any): Boolean = other match {
    case that: ScExistentialType =>
      quantified == that.quantified
    case _ => false
  }

  //to make it different from `quantified.hashCode`
  override def hashCode(): Int = quantified #+ ScExistentialType
}

object ScExistentialType {

  def unapply(existential: ScExistentialType): Option[(ScType, List[ScExistentialArgument])] =
    Some((existential.quantified, existential.wildcards))

  /** Specification 3.2.10:
    * 1. Multiple for-clauses in an existential type can be merged. E.g.,
    * T forSome {Q} forSome {H} is equivalent to T forSome {Q;H}.
    * 2. Unused quantifications can be dropped. E.g., T forSome {Q;H} where
    * none of the types defined in H are referred to by T or Q, is equivalent to
    * T forSome {Q}.
    * 3. An empty quantification can be dropped. E.g., T forSome { } is equivalent
    * to T.
    * 4. An existential type T forSome {Q} where Q contains a clause
    * type t[tps] >: L <: U is equivalent to the type T' forSome {Q} where
    * T' results from T by replacing every covariant occurrence (4.5) of t in T by
    * U and by replacing every contravariant occurrence of t in T by L.
    *
    * 1. and 2. are always true by construction.
    * Simplification by 3. and 4. is computed once when existential type is created.
    */
  @tailrec
  final def apply(
    quantified:        ScType,
    precalculatedArgs: Option[List[ScExistentialArgument]] = None
  ): ScExistentialType = quantified match {
      case e: ScExistentialType =>
        //first rule
        ScExistentialType(e.quantified, precalculatedArgs)
      case _ =>
        //second rule
        val args = precalculatedArgs.getOrElse(notBoundArgs(quantified).toList)
        new ScExistentialType(quantified, args, simplify(quantified, args))
    }

  private def simplify(quantified: ScType, wildcards: List[ScExistentialArgument], visitedQ: Set[ScType] = Set.empty): Option[ScType] = {
    val startVariance = quantified match {
      case _: ScExistentialArgument | _: ScParameterizedType => Covariant //why?
      case _                                                 => Invariant
    }

    //third rule
    if (wildcards.isEmpty) return Some(quantified)

    var updated = false
    //fourth rule
    val simplifiedQ = quantified.recursiveVarianceUpdate(startVariance) {
      case (ex: ScExistentialType, _) =>
        if (ex.simplified.nonEmpty) {
          updated = true
        }
        ReplaceWith(ex.simplify())
      case (arg: ScExistentialArgument, variance) =>
        if (arg.isDeferred) Stop
        else {
          //fourth rule
          val argOrBound = variance match {
            case Covariant =>
              updated = true
              arg.upper
            case Contravariant =>
              updated = true
              arg.lower
            case _ =>
              arg
          }
          ReplaceWith(argOrBound)
        }
      case _ => ProcessSubtypes
    }

    if (!updated) None
    else {
      val newWildcards = notBoundArgs(simplifiedQ).toList
      if (newWildcards.isEmpty)
        Some(simplifiedQ)
      else if (visitedQ.contains(simplifiedQ))
        None
      //semi-invariant to avoid infinite recursive building of "simplified" types, like in example (T forSome {type T <: C[T]})
      else if (newWildcards.size < wildcards.size || simplifiedQ.typeDepth <= quantified.typeDepth)
        Some(new ScExistentialType(simplifiedQ, newWildcards, simplify(simplifiedQ, newWildcards, visitedQ + quantified)))
      else
        None
    }
  }

  private def notBoundArgs(tp: ScType): Set[ScExistentialArgument] = {
    var result: Set[ScExistentialArgument] = Set.empty
    tp.recursiveUpdate {
      case _: ScExistentialType => Stop //arguments inside are considered bound
      case arg: ScExistentialArgument =>
        if (result.contains(arg)) Stop
        else {
          result += arg

          if (arg.isDeferred) Stop
          else                ProcessSubtypes
        }
      case _ =>
        ProcessSubtypes
    }
    result
  }

  private def equivImpl(left : ScExistentialType,
                        right: ScExistentialType,
                        constraints: ConstraintSystem,
                        falseUndef: Boolean): ConstraintsResult = {

    val rightToLeft: java.util.Map[ScExistentialArgument, ScExistentialArgument] = {
      val byName: TObjectHashingStrategy[ScExistentialArgument] = new TObjectHashingStrategy[ScExistentialArgument] {
        override def computeHashCode(t: ScExistentialArgument): Int = t.name.hashCode
        override def equals(t: ScExistentialArgument, t1: ScExistentialArgument): Boolean = t.name == t1.name
      }
      val map = new THashMap[ScExistentialArgument, ScExistentialArgument](byName)
      right.wildcards.zip(left.wildcards).foreach {
        case (x, y) => map.put(x, y)
      }
      map
    }

    def replaceRightToLeft(rightSubtype: ScType): ScType = rightSubtype.updateRecursively {
      case arg: ScExistentialArgument =>
        rightToLeft.getOrDefault(arg, arg)
    }

    var lastConstraints = constraints
    val leftIterator = left.wildcards.iterator
    val rightIterator = right.wildcards.iterator

    while (leftIterator.hasNext && rightIterator.hasNext) {
      val left = leftIterator.next()
      val right = rightIterator.next()

      val subst = ScSubstitutor.bind(right.typeParameters, left.typeParameters)(TypeParameterType(_))
      val updatedRightLower = replaceRightToLeft(subst(right.lower))

      val lowerResult = left.lower.equiv(updatedRightLower, lastConstraints, falseUndef)
      if (lowerResult.isLeft)
        return ConstraintsResult.Left

      lastConstraints = lowerResult.constraints

      val updatedRightUpper = replaceRightToLeft(subst(right.upper))
      val upperResult = left.upper.equiv(updatedRightUpper, lastConstraints, falseUndef)

      if (upperResult.isLeft)
        return ConstraintsResult.Left

      lastConstraints = upperResult.constraints
    }

    val updatedRightQuantified = replaceRightToLeft(right.quantified)
    updatedRightQuantified.equiv(left.quantified, constraints, falseUndef)
  }

}