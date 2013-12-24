/*
 * Iteratee.scala
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


package com.example.iteratee.pure


sealed trait Iteratee[In, Out] {

  /**
   * Yields the result of this iteratee. If the iteratee is in the continuation
   * state when run, a final EOF is fed to the iteratee in order to give a
   * chance to produce a result. Failure to do so consequently results in a
   * "diverging iteratee" error.
   */
  def run: Out = this match {
    case Done(result, _) => result
    case Error(t)        => throw t
    case Cont(k)         => k(EOF) match {
        case Done(result, _) => result
        case Error(t)        => throw t
        case _               => sys.error("Diverging iteratee!")
      }
  }

  /**
   * Apply the given function to the result of this iteratee.
   */
  def map[NewOut](f: Out => NewOut): Iteratee[In, NewOut]

  /**
   * Apply the given function to the result of this iteratee and flatten.
   */
  def flatMap[NewOut](f: Out => Iteratee[In, NewOut]): Iteratee[In, NewOut]

}

object Iteratee extends IterateeFunctions {

  /**
   * A new iteratee based on the given functions which handle Element, Empty
   * and EOF. This method is provided merely for convenience in order to avoid
   * redundant pattern matching when defining new iteratees.
   * @param el evaluated whenever a new element from the input stream is fed to the iteratee
   * @param empty evaluated whenever there is no new element from the input stream
   * @param eof evaluated when there is (and will be) no more data from the input stream
   */
  def apply[In, Out](el:    In => Iteratee[In, Out],
                     empty: => Iteratee[In, Out],
                     eof:   => Iteratee[In, Out]): Iteratee[In, Out] = {
    def step: Input[In] => Iteratee[In, Out] = {
      case EOF => eof
      case Empty => empty
      case Element(x) => el(x)
    }
    Cont(step)
  }

}

case class Done[In, Out](result: Out, remainingInput: Input[In]) extends Iteratee[In, Out] {

  def map[NewOut](f: Out => NewOut): Iteratee[In, NewOut] =
    /* Apply the given function to the result of this iteratee. */
    Done(f(result), remainingInput)

  def flatMap[NewOut](f: Out => Iteratee[In, NewOut]): Iteratee[In, NewOut] =
    /*
     * Application of the given function to the result of this iteratee yields a
     * new iteratee. If the latter is again in Done state, return its result
     * along with our remaining input. For one, that iteratee hasn't seen any
     * input elements from the original input stream and was solely created
     * based on our accumulated result. Returning our remaining input along with
     * the new result is also mandatory for composition. Composition is also the
     * reason why we feed our remaining input to k in the Cont case.
     */
    f(result) match {
      case Done(newResult, _) => Done(newResult, remainingInput)
      case Cont(k)            => k(remainingInput)
      case Error(t)           => Error[In, NewOut](t)
    }

}

case class Cont[In, Out](k: Input[In] => Iteratee[In, Out]) extends Iteratee[In, Out] {

  def map[NewOut](f: Out => NewOut): Iteratee[In, NewOut] =
    /*
     * Since we have no result which we could map over yet we can only return a
     * continuation that involves a new step function that takes an input
     * element, feeds it to our current step function (k) and calls map on the
     * resulting iteratee.
     */
    Cont(x => k(x) map f)

  def flatMap[NewOut](f: Out => Iteratee[In, NewOut]): Iteratee[In, NewOut] =
    /*
     * Since we have no result which we could map over yet we can only return a
     * continuation that involves a new step function that takes an input
     * element, feeds it to our current step function (k) and calls flatMap on
     * the resulting iteratee.
     */
    Cont(x => k(x) flatMap f)

}

case class Error[In, Out](t: Throwable) extends Iteratee[In, Out] {

  def map[NewOut](f: Out => NewOut): Iteratee[In, NewOut] =
    this.asInstanceOf[Iteratee[In, NewOut]]

  def flatMap[NewOut](f: Out => Iteratee[In, NewOut]): Iteratee[In, NewOut] =
    this.asInstanceOf[Iteratee[In, NewOut]]

}
