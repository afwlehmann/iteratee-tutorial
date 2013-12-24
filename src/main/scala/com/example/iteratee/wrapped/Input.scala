/*
 * Input.scala
 * copyright (c) 2013 by Alexander Lehmann <afwlehmann@googlemail.com>
 */


package com.example.iteratee.wrapped


sealed trait Input[+T]


case class  Element[T](x: T) extends Input[T]

case object Empty            extends Input[Nothing]

case object EOF              extends Input[Nothing]
