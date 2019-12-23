package com.chepelov.ballclock

import cats.Monoid

import scala.language.implicitConversions

package object kata {
  private case class PairMonoid[A, B]()(implicit aMon: Monoid[A], bMon: Monoid[B]) extends Monoid[(A, B)] {
    override def empty: (A, B) = (aMon.empty, bMon.empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) =
      (x, y) match {
        case ((xa, xb), (ya, yb)) =>
          (aMon.combine(xa, ya), bMon.combine(xb, yb))
      }
  }
  
  implicit def toPairMonoid[A: Monoid, B: Monoid]: Monoid[(A,B)] = PairMonoid[A, B]()


  private case class TripletMonoid[A, B, C]()(implicit aMon: Monoid[A],
                                              bMon: Monoid[B],
                                              cMon: Monoid[C]) extends Monoid[(A, B, C)] {
    override def empty: (A, B, C) = (aMon.empty, bMon.empty, cMon.empty)

    override def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) =
      (x, y) match {
        case ((xa, xb, xc), (ya, yb, yc)) =>
          (aMon.combine(xa, ya), bMon.combine(xb, yb), cMon.combine(xc, yc))
      }
  }

  implicit def toTripletMonoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] = TripletMonoid[A, B, C]()

}

