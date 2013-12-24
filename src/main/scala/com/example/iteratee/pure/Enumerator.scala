/*
 * Enumerator.scala
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


package com.example.iteratee.pure


sealed trait Enumerator[From] {
  /**
   * Apply this enumerator to the given iteratee, i.e. fold the iteratee over
   * the input stream represented by this enumerator.
   */
  def apply[To](iter: Iteratee[From, To]): Iteratee[From, To]

  /**
   * Apply this enumerator to the given iteratee, then call run on the resulting
   * iteratee. If applicable, the latter will feed a final EOF to the iteratee.
   */
  def run[To](iter: Iteratee[From, To]): To =
    apply(iter).run
}

object Enumerator {

  /**
   * Enumerate over a given sequence of elements.
   */
  def apply[From](s: Seq[From]): Enumerator[From] =
    new Enumerator[From] {
      override def apply[To](iter: Iteratee[From, To]): Iteratee[From, To] = {
        /*
         * Use this auxiliary function to enumerate manually instead of using
         * foldLeft & friends. This way enumeration can stop early once e.g.
         * the iteratee is in either Done or Error state.
         */
        def enum(xs: Seq[From], it: Iteratee[From, To]): Iteratee[From, To] = {
          (xs.isEmpty, it) match {
            case (false, Cont(k)) => enum(xs.tail, k(Element(xs.head)))
            case (_, i)           => i
          }
        }
        enum(s, iter)
      }
    }

  /**
   * Enumerate over non-empty varargs.
   */
  def apply[From](x: From, xs: From*): Enumerator[From] =
    Enumerator(x +: xs)

}
