/*
 * examples.sc
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


import com.example.iteratee.pure._
import Iteratee._


// Prints every input element to the console.
def putStrLn[T]: Iteratee[T, Unit] = {
  def step: Input[T] => Iteratee[T, Unit] = {
    case EOF        => Done((), EOF)
    case Empty      => Cont(step)
    case Element(x) => println(x); Cont(step) // <-- side-effect like a boss!
  }
  Cont(step)
}

Enumerator(List(1,2,3)).run(putStrLn)

Enumerator("PING".toList).run(putStrLn)


// Yields the second element of the input stream (composition!).
def drop1Keep1[T] = for {
  _ <- drop[T](1)
  x <- head[T]
} yield x

Enumerator("Hello world!".toList).run(drop1Keep1)


// More composition.
def pair[T] = for {
  a <- head[T]
  b <- head[T]
} yield (a, b)

Enumerator("Hello world!".toList).run(pair)


// The power of `sequence` and `repeat`.

def fivePairs[T]  = sequence { List.fill(5)(pair[Int]) }

def alternates[T] = repeat   { drop1Keep1[T] }

val enum: Enumerator[Int] = Enumerator(1 to 10)


enum.run(fivePairs)

enum.run(alternates).toList


// Determine the length of two input streams.
val finalIter: Iteratee[Char,Int] = {
  val iter1 = Enumerator("Hello ".toList)(length)
  val iter2 = Enumerator("world!".toList)(iter1)
  iter2
}

finalIter.run


// Demonstrate takeWhile and dropWhile.
enum.run(takeWhile[Int]((_ < 3))).toList
enum.run {
  for {
    _ <- dropWhile[Int]((_ < 3))
    x <- head[Int]
  } yield x
}


// Create a new iteratee using Iteratee.apply for convenience.
val odds: Iteratee[Int, Stream[Int]] = {
  def step(acc: Stream[Int]): Iteratee[Int, Stream[Int]] = Iteratee(
    el    = { x => if (x % 2 == 1) step(acc ++ Stream(x)) else step(acc) },
    empty = step(acc),
    eof   = Done(acc, EOF)
  )
  step(Stream.empty)
}

enum.run(odds).toList


// Enumeratee's map, apply and transform.
val sum = Iteratee.fold[Int, Int](0) { case (acc, elt) => acc + elt }
val em: Enumeratee[String, Int] = Enumeratee map (Integer.parseInt(_))

Enumerator("1","2","3").run(em(sum))

val sumTransformed: Iteratee[String, Int] = em transform sum
Enumerator("1","2","3") run sumTransformed


// Enumerator's through.
Enumerator("1","2","3") through em run sum


// Enumeratee's take, drop, takeWhile, dropWhile and filter.
Enumerator(1 to 10).run(Enumeratee.take(5) transform sum)
Enumerator(1 to 10).run(Enumeratee.drop(5) transform sum)
Enumerator(1 to 10).run(Enumeratee.takeWhile[Int](_ <= 5) transform sum)
Enumerator(1 to 10).run(Enumeratee.dropWhile[Int](_ <= 5) transform sum)
Enumerator(1 to 10).run(Enumeratee.filter[Int](_ % 2 == 1) transform sum)
