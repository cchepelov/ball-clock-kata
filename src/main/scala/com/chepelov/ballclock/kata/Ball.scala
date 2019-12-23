package com.chepelov.ballclock.kata

import cats.Order

case class Ball(id: Int) extends AnyVal {
  override def toString: String = id match {
    case _ if id >= 0 && id <= 50 =>
      "" + Ball.UnicodeBalls(id)
    case _ if id <= -1 && id >= -20 =>
      "" + Ball.UnicodeNegativeBalls( -(id+1) )
    case _ =>
      s"(${id})"
  }

}
object Ball {
  implicit val order: Order[Ball] = Order.fromOrdering(Ordering.by(_.id))

  private val UnicodeBalls =
    "⓪①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳㉑㉒㉓㉔㉕㉖㉗㉘㉙㉚㉛㉜㉝㉞㉟㊱㊲㊳㊴㊵㊶㊷㊸㊹㊺㊻㊼㊽㊾㊿"

  private val UnicodeNegativeBalls =
    "❶❷❸❹❺❻❼❽❾❿⓫⓬⓭⓮⓯⓰⓱⓲⓳⓴"
}