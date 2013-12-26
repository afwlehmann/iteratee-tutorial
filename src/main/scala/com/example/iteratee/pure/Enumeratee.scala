/*
 * Enumeratee.scala
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


package com.example.iteratee.pure


trait Enumeratee[From, To] {

  def apply[TT](iter: Iteratee[To, TT]): Iteratee[From, Iteratee[To, TT]]

  def transform[TT](iter: Iteratee[To, TT]): Iteratee[From, TT] =
    apply(iter).flatten[TT]

}


object Enumeratee {

  // Idea taken from the play framework
  sealed trait CheckDone[F,T] extends Enumeratee[F,T] {
    def continue[R](k: Input[T] => Iteratee[T, R]): Iteratee[F, Iteratee[T, R]]

    def apply[R](iter: Iteratee[T, R]): Iteratee[F, Iteratee[T, R]] =
      iter match {
        case Cont(k) => continue(k)
        case _       => Done(iter, Empty)
      }
  }

  /**
   * Pass on untransformed input.
   */
  def identity[F]: Enumeratee[F,F] = new CheckDone[F,F] {
    def step[R](k: Input[F] => Iteratee[F,R]): Input[F] => Iteratee[F, Iteratee[F,R]] = {
      case in @ (Element(_) | Empty) => new CheckDone[F,F] {
        def continue[RR](k: Input[F] => Iteratee[F,RR]): Iteratee[F, Iteratee[F,RR]] =
          Cont(step(k))
      } apply k(in)
      case EOF => Done(Cont(k), EOF)
    }

    def continue[R](k: (Input[F]) => Iteratee[F,R]): Iteratee[F, Iteratee[F,R]] =
      Cont(step(k))
  }

  /**
   * Pass on input transformed by the given function `f`.
   */
  def map[F,T](f: F => T): Enumeratee[F,T] = new CheckDone[F,T] {
    def continue[TT](k: Input[T] => Iteratee[T, TT]): Iteratee[F, Iteratee[T, TT]] = {
      def step: Input[F] => Iteratee[F, Iteratee[T, TT]] = {
        case EOF        => Done(k(EOF), EOF)
        case Empty      => Cont(step)
        case Element(x) => k(Element(f(x))) match {
          case Cont(k) => continue(k)
          case i @ _   => Done(i, Empty)
        }
      }
      Cont(step)
    }
  }

  /**
   * Pass on only the first `n` elements of the input stream.
   */
  def take[F](n: Int): Enumeratee[F,F] = new CheckDone[F,F] {
    def step[R](nPrime: Int, k: Input[F] => Iteratee[F,R]): Input[F] => Iteratee[F, Iteratee[F,R]] = {
      case EOF                       => Done(Cont(k), EOF)
      case Empty                     => Cont(step(nPrime, k))
      case Element(x) if nPrime <= 0 => Done(Cont(k), Element(x))
      case Element(x)                => new CheckDone[F,F] {
        def continue[RR](k: (Input[F]) => Iteratee[F,RR]): Iteratee[F, Iteratee[F,RR]] =
          Cont(step(nPrime - 1, k))
      } apply k(Element(x))
    }

    def continue[T](k: Input[F] => Iteratee[F, T]): Iteratee[F, Iteratee[F, T]] =
      Cont(step(n, k))
  }

  /**
   * Pass on elements of the input stream as long as they fulfill the given predicate `p`.
   */
  def takeWhile[F](p: F => Boolean): Enumeratee[F,F] = new CheckDone[F,F] {
    def step[T](k: Input[F] => Iteratee[F, T]): Input[F] => Iteratee[F, Iteratee[F, T]] = {
      case EOF                => Done(Cont(k), EOF)
      case Empty              => Cont(step(k))
      case Element(x) if p(x) => new CheckDone[F,F] {
        def continue[TT](k: Input[F] => Iteratee[F, TT]): Iteratee[F, Iteratee[F, TT]] =
          Cont(step(k))
      } apply k(Element(x))
      case Element(_)         => Cont(step(k))
    }

    def continue[R](k: (Input[F]) => Iteratee[F, R]): Iteratee[F, Iteratee[F, R]] =
      Cont(step(k))
  }

  /**
   * Drop the first `n` elements of the input stream, then pass on all remaining chunks.
   */
  def drop[F](n: Int): Enumeratee[F,F] = new CheckDone[F,F] {
    def step[R](nPrime: Int, k: Input[F] => Iteratee[F,R]): Input[F] => Iteratee[F, Iteratee[F,R]] = {
      case EOF        => Done(Cont(k), EOF)
      case Empty      => Cont(step(nPrime, k))
      case Element(_) => drop(n-1) apply Cont(k)
    }

    def continue[R](k: Input[F] => Iteratee[F,R]): Iteratee[F, Iteratee[F,R]] =
      if (n <= 0) identity[F](Cont(k)) else Cont(step(n, k))
  }

  /**
   * Drop elements of the input stream as long as they fulfill the given predicate `p`,
   * then pass on all remaining chunks.
   */
  def dropWhile[F](p: F => Boolean): Enumeratee[F,F] = new CheckDone[F,F] {
    def step[T](k: Input[F] => Iteratee[F,T]): Input[F] => Iteratee[F, Iteratee[F,T]] = {
      case EOF                => Done(Cont(k), EOF)
      case Empty              => Cont(step(k))
      case Element(x) if p(x) => dropWhile(p) apply Cont(k)
      case Element(x)         => identity apply k(Element(x))
    }

    def continue[R](k: (Input[F]) => Iteratee[F, R]): Iteratee[F, Iteratee[F, R]] =
      Cont(step(k))
  }

  /**
   * Pass on only those elements that fulfill the given predicate `p`.
   */
  def filter[F](p: F => Boolean): Enumeratee[F,F] = new CheckDone[F,F] {
    def step[T](k: Input[F] => Iteratee[F,T]): Input[F] => Iteratee[F, Iteratee[F,T]] = {
      case EOF                => Done(Cont(k), EOF)
      case Empty              => Cont(step(k))
      case Element(x) if p(x) => new CheckDone[F,F] {
        def continue[R](k: (Input[F]) => Iteratee[F, R]): Iteratee[F, Iteratee[F, R]] =
          Cont(step(k))
      } apply k(Element(x))
      case Element(_)         => Cont(step(k))
    }

    def continue[R](k: (Input[F]) => Iteratee[F, R]): Iteratee[F, Iteratee[F, R]] =
      Cont(step(k))
  }

}
