package com.chepelov.ballclock.kata

import java.time.Duration

import zio.{IO, Schedule, UIO, ZIO}
import cats.Foldable
import cats.instances.order
import cats.implicits._
import cats.Monoid
import zio.stream.ZStream

import scala.annotation.tailrec

case class BallClock private (stages: List[Accumulator.Tilting], end: Accumulator.Static) {
  def countUntilCycle(): IO[KataError, Duration] = {

    val start: IO[KataError, (BallClock, Long)] = this.tick.map(_ -> 1L)

    val innerStream = ZStream.iterate(start)(stateIo => stateIo.flatMap { case (state, count) =>
      state.tick.map(newState => newState -> (count + 1L ))
    })


    val result = innerStream
      .flatMap(io => ZStream.fromEffect[Any, KataError, (BallClock, Long)](io))
      .takeUntil {
        case (state, count) => state == this
      }
      .runLast
      .foldM(error => IO.fail(error), {
        case None =>
          IO.fail(KataError.CycleNotFound)
        case Some((_, count)) =>
          IO.succeed( Duration.ofMinutes(count))
      })

    result
  }


  /**
   * Load a new set of balls within the ball clock, assigning them new numbers
   *
   * @return the new state of the clock
   */
  def load(ballCount: Int): IO[KataError, BallClock] = {
    import BallClock.MaxCapacity

    val (existingCapacity, existingSize, staticSize) =
      Monoid[(Int, Int, Int)].combineAll(stages.map(st => (st.capacity, st.size, st.static.size)))

    if ((existingSize + ballCount) < (existingCapacity - staticSize)) {
      IO.fail(KataError.InsufficientBallCount(ballCount, existingCapacity, existingSize))
    } else if ((existingSize + ballCount) > (MaxCapacity + staticSize)) {
      IO.fail(KataError.ExcessiveBallCount(ballCount, existingCapacity, existingSize, MaxCapacity + staticSize))
    } else {

      val maxExistingBall = (end :: stages).flatMap(_.contents.to[List].maximumOption).maximumOption

      val newBalls = (maxExistingBall.map(_.id).getOrElse(0) until ballCount).map(num => Ball(num + 1))
      val newEnd = newBalls.foldLeft(end) { case (endchute, ball) =>
        val (newState, _) = endchute.receive(ball)
        newState
      }
      UIO(this.copy(end = newEnd))
    }
  }


  /**
   * Perform all the motions related to one motion of the rotating arm (pick a ball,
   * @return
   */
  def tick: IO[KataError, BallClock] = {

    for {
      first <- end.takeFirst
    } yield {
      val (movingBall, initialNewEnd) = first
      val (newStages, newEnd) = BallClock.stageMotion(stages, Some(movingBall), initialNewEnd, Nil, Nil)
      this.copy(stages = newStages, end = newEnd)
    }
  }

}

object BallClock {
  @tailrec
  private def stageMotion(stages: List[Accumulator.Tilting], maybeBall: Option[Ball],
                  end: Accumulator.Static,
                  towardsEnd: Seq[Ball] = Nil, newStagesReversed: List[Accumulator.Tilting]):
  (List[Accumulator.Tilting], Accumulator.Static) = {
    stages match {
      case Nil =>
        // everything that might be moving, is moving. Time to collect every ball hurtling towards
        // the static accumulator. Starting with the ball moving from the last tilt, if any.
        val toReceive = maybeBall match {
          case Some(ball) => ball :: towardsEnd.to[List]
          case None => towardsEnd.to[List]
        }

        val newEnd = toReceive.foldLeft(end) { case (endState, ball) =>
          val (newEndState, None) = endState.receive(ball)
          newEndState
        }

        (newStagesReversed.reverse, newEnd)

      case currentStage :: otherStages =>
        maybeBall match {
          case None =>
            // No more ball moving? Nothing will happen in the lower stages, except the end stage.
            stageMotion(otherStages, None, end, towardsEnd, currentStage :: newStagesReversed)

          case Some(ball) =>
            currentStage.receive(ball) match {
              case (newCurrentStage, None) =>
                // There will be no subsequent ball motion, but we did affect the current stage's state
                stageMotion(otherStages, None, end, towardsEnd, newCurrentStage :: newStagesReversed)

              case (newCurrentStage, Some((newMovingBall, newRejectedBalls))) =>
                stageMotion(otherStages,
                  Some(newMovingBall),
                  end,
                  towardsEnd ++ newRejectedBalls,
                   // newRejectedBalls ++ towardsEnd,
                  /* FIXME: in which order are rejected balls received in case of a multiple tilt event ?
                  *   BallClock geometry is important here! */
                  newCurrentStage :: newStagesReversed)
            }
        }
    }
  }

  val MaxCapacity: Int = 127



  def apply(): BallClock =
    BallClock(
      Accumulator.Tilting("Min", Nil, 5) ::
        Accumulator.Tilting("5Min", Nil, 12) ::
        Accumulator.Tilting("Hour", Nil, Ball(-1) :: Nil, 12) ::
      Nil,
      Accumulator.Static.Empty("Main")
    )
}