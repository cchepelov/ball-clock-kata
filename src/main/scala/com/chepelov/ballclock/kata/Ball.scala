package com.chepelov.ballclock.kata

import cats.Order
import play.api.libs.json.{JsNumber, JsValue, Writes}

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


  implicit object JsonWrites extends Writes[Ball] {
    override def writes(o: Ball): JsValue = JsNumber(o.id)
  }
}