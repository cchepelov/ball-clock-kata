package com.chepelov.ballclock.kata

sealed trait KataError
object KataError {
  case object Empty extends KataError
  case object CycleNotFound extends KataError
  case class UnknownError(underlying: Any) extends KataError

  case class UnderlyingError(t: Throwable) extends KataError
  case class InsufficientBallCount(provided: Int, existing: Int, tiltingCapacity: Int) extends KataError
  case class ExcessiveBallCount(provided: Int, existing: Int, tiltingCapacity: Int, maxSystemCapacity: Int) extends KataError
}