/*
 * IterateeFunctions.scala
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


package com.example.iteratee.pure


/**
 * Utility functions for the companion object of Iteratee.
 */
trait IterateeFunctions {

  def length[T]: Iteratee[T, Int] = {
    def step(n: Int): Input[T] => Iteratee[T, Int] = {
      case EOF        => Done(n, EOF)
      case Empty      => Cont(step(n))
      case Element(_) => Cont(step(n+1))
    }
    Cont(step(0))
  }

  def head[T]: Iteratee[T, T] = {
    def step: Input[T] => Iteratee[T, T] = {
      case EOF        => Error(new NoSuchElementException)
      case Empty      => Cont(step)
      case Element(x) => Done(x, Empty)
    }
    Cont(step)
  }

  // Use Enumeratee.drop instead.
  def drop[T](n: Int): Iteratee[T, Unit] = {
    def step: Input[T] => Iteratee[T, Unit] = {
      case EOF        => Done((), EOF)
      case Empty      => Cont(step)
      case Element(_) => drop(n-1)
    }
    if (n <= 0) Done((), Empty) else Cont(step)
  }

  // Use Enumeratee.filter instead.
  def filter[T](p: T => Boolean): Iteratee[T, Stream[T]] = {
    def step(acc: Stream[T]): Input[T] => Iteratee[T, Stream[T]] = {
      case EOF        => Done(acc, EOF)
      case Empty      => Cont(step(acc))
      case Element(x) => if (p(x)) Cont(step(acc ++ Stream(x))) else Cont(step(acc))
    }
    Cont(step(Stream.empty[T]))
  }

  // Use Enumeratee.takeWhile instead.
  def takeWhile[T](p: T => Boolean): Iteratee[T, Stream[T]] = {
    def step(acc: Stream[T]): Input[T] => Iteratee[T, Stream[T]] = {
      case EOF        => Done(acc, EOF)
      case Empty      => Cont(step(acc))
      case Element(x) => if (p(x)) Cont(step(acc ++ Stream(x))) else Done(acc, Empty)
    }
    Cont(step(Stream.empty[T]))
  }

  // Use Enumeratee.dropWhile instead.
  def dropWhile[T](p: T => Boolean): Iteratee[T, Unit] = {
    def step: Input[T] => Iteratee[T, Unit] = {
      case EOF        => Done((), EOF)
      case Empty      => Cont(step)
      case Element(x) => if (p(x)) Cont(step) else Done((), Element(x))
    }
    Cont(step)
  }

  /**
   * Transform a list of iteratees to an iteratee whose result is a list of the
   * results of the given iteratees. Defining this for List seems reasonable and
   * avoids the havoc caused by CanBuildFrom.
   */
  def sequence[In, Out](xs: List[Iteratee[In, Out]]): Iteratee[In, List[Out]] = {
    xs.foldLeft[Iteratee[In, List[Out]]](Done(List.empty[Out], Empty)) { case (acc, iter) =>
      for {
        h <- iter
        t <- acc
      } yield h :: t
    }
  }

  /**
   * Repeat the given iteratee indefinitely and accumulate the result as a stream.
   */
  def repeat[In, Out](iter: Iteratee[In, Out]): Iteratee[In, Stream[Out]] = {
    def step(acc: Stream[Out]): Input[In] => Iteratee[In, Stream[Out]] = {
      case EOF        => Done(acc, EOF)
      case Empty      => Cont(step(acc))
      case Element(x) => iter match {
        case Done(result, _) => Done(acc ++ Stream(result), Element(x))
        case Error(t)        => Error[In, Stream[Out]](t)
        case Cont(k)         => for {
          h <- k(Element(x))
          t <- repeat(iter)
        } yield acc ++ (h #:: t)
      }
    }
    Cont(step(Stream.empty[Out]))
  }

  /**
   * Folds over input streams (this is basically a foldLeft).
   */
  def fold[In, Out](z: Out)(f: (Out, In) => Out): Iteratee[In, Out] = {
    def step(acc: Out): Input[In] => Iteratee[In, Out] = {
      case Element(x) => Cont(step(f(acc, x)))
      case Empty      => Cont(step(acc))
      case EOF        => Done(acc, EOF)
    }
    Cont(step(z))
  }

}
