package com.chepelov.ballclock.kata

import cats.Order

case class Ball(id: Int) extends AnyVal
object Ball {
  implicit val order: Order[Ball] = Order.fromOrdering(Ordering.by(_.id))
}