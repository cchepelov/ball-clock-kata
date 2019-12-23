package com.chepelov.ballclock.kata

import zio.{IO, UIO}

/**
 * This class represents a ball accumulator, with optional tilting.
 */
sealed abstract class Accumulator {
  def name: String
  def contents: Seq[Ball]
  def size: Int

  /**
   * Receive a ball and optionally performs a Tilt (which empties the accumulator, singles out
   * one ball for delivery to the next accumulator, and drops every other ball to the main accumulator)
   *
   * @return the new state of the accumulator and optionally a lot of balls:
   *         first the last received balls, then the balls that were previously accumulated, in reverse
   *         receive order (i.e. the head is the (second to) LAST received ball, etc.)
   */
  def receive(ball: Ball): (Accumulator, Option[(Ball, Seq[Ball])])
}

object Accumulator {

  /**
   * This class represents a Static accumulator, i.e. an accumulator that does *not* ever tilt
   *
   * Contrary to other accumulator, it is possible to take the ball at the head
   */
  abstract class Static extends Accumulator {
    override def receive(ball: Ball): (Accumulator.Static.NonEmpty, None.type)

    /**
     * Attempt to take the first ball, returning an error effect if necessary
     */
    def takeFirst: IO[KataError, (Ball, Accumulator.Static)]
  }

  object Static {
    case class Empty(val name: String) extends Static {
      override def size: Int = 0
      override def contents: Seq[Ball] = Nil

      override def receive(ball: Ball): (Accumulator.Static.NonEmpty, None.type) =
        NonEmpty(name, Vector(ball)) -> None

      def takeFirst: IO[KataError.Empty.type, Nothing] = IO.fail(KataError.Empty)
    }

    case class NonEmpty(override val name: String, contents: Vector[Ball]) extends Static {
      override def size: Int = contents.size

      override def receive(ball: Ball): (Accumulator.Static.NonEmpty, None.type) =
        this.copy(contents = ball +: contents) -> None

      def takeFirst: IO[Nothing, (Ball, Accumulator.Static)] = UIO {
        if (size == 1) {
          contents.head -> Empty(name)
        } else {
          (contents.last, this.copy(contents = contents.init))
        }
      }
    }

    def apply(name: String, contents: Seq[Ball]): Static =
      if (contents.isEmpty) {
        Empty(name)
      } else {
        NonEmpty(name, contents.to[Vector])
      }
  }


  /**
   * This class represents a Tilting accumulator, i.e. an accumulator that will tilt once it
   * reaches capacity, dropping one ball towards the next accumulator and the other balls towards the
   * Main accumulator (in reverse order, i.e. the second-to-last receive ball goes first into the
   * Main)
   *
   */
  final case class Tilting(name: String, moving: List[Ball], static: List[Ball], capacity: Int) extends Accumulator {

    def contents: Seq[Ball] = moving ++ static
    def size: Int = moving.size + static.size

    override def receive(ball: Ball): (Accumulator.Tilting, Option[(Ball, List[Ball])]) =
      if ((size + 1) < capacity) {
        this.copy(moving = ball :: moving) -> None  // no tilt yet
      } else {
        this.copy(moving = Nil) -> Some(ball -> moving) // we leave the static balls glued in place
      }
  }
  object Tilting {
    def apply(name: String, moving: Seq[Ball], capacity: Int): Tilting = Tilting(name, moving.to[List], Nil, capacity)
  }
}
