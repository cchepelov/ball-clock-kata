package com.chepelov.ballclock.kata

import java.time.Duration

import zio.{IO, Schedule, UIO, ZIO}
import cats.Foldable
import cats.instances.order
import cats.implicits._
import cats.Monoid
import zio.stream.ZStream

import scala.annotation.tailrec

sealed abstract class BallClock {
  def stages: List[Accumulator.Tilting]
  def end: Accumulator.Static

  /**
   * Load a new set of balls within the ball clock, assigning them new numbers
   *
   * @return the new state of the clock
   */
  def load(ballCount: Int): IO[KataError, BallClock.Runnable] = {
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
      val newEnd = BallClock.feedBallsIntoMain(end, newBalls.to[List])
      UIO(BallClock.Runnable(stages = this.stages, end = newEnd))
    }
  }
}


object BallClock {

  private def feedBallsIntoMain(end: Accumulator.Static, toReceive: List[Ball]): Accumulator.Static.NonEmpty =
    end match {
      case nonEmptyEnd: Accumulator.Static.NonEmpty =>
        toReceive match {
          case Nil =>
            nonEmptyEnd
          case single :: Nil =>
            val (newEndState, None) = nonEmptyEnd.receive(single)
            newEndState

          case _ =>
            toReceive.foldLeft(nonEmptyEnd) { case (endState, ball) =>
              val (newEndState, None) = endState.receive(ball)
              newEndState
            }
        }
      case emptyEnd: Accumulator.Static.Empty =>
        toReceive match {
          case Nil =>
            throw new IllegalStateException("we should never reach here — catastrophic failure") // we could put that into the monad if there was an errorable monadic Schedule.unfoldM
          case singleBall :: Nil =>
            val (newEndState, None) = emptyEnd.receive(singleBall)
            newEndState

          case firstBall :: otherBalls =>
            val (firstNewEndState, None) = emptyEnd.receive(firstBall)

            otherBalls.foldLeft(firstNewEndState) { case (endState, ball) =>
              val (newEndState, None) = endState.receive(ball)
              newEndState
            }
        }
    }



  @tailrec
  private def stageMotion(stages: List[Accumulator.Tilting], maybeBall: Option[Ball],
                          end: Accumulator.Static,
                          towardsEnd: Seq[Ball] = Nil, newStagesReversed: List[Accumulator.Tilting]):
  (List[Accumulator.Tilting], Accumulator.Static.NonEmpty) = {
    stages match {
      case Nil =>
        // everything that might be moving, is moving. Time to collect every ball hurtling towards
        // the static accumulator. Starting with the ball moving from the last tilt, if any.
        val toReceive = maybeBall match {
          case Some(ball) => ball :: towardsEnd.to[List]
          case None => towardsEnd.to[List]
        }

        val newEnd = feedBallsIntoMain(end, toReceive)
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
                  //towardsEnd ++ newRejectedBalls,
                   newRejectedBalls ++ towardsEnd,
                  /* FIXME: in which order are rejected balls received in case of a multiple tilt event ?
                  *   BallClock geometry is important here! */
                  newCurrentStage :: newStagesReversed)
            }
        }
    }
  }

  val MaxCapacity: Int = 127

  private case class NotRunnable(stages: List[Accumulator.Tilting], end: Accumulator.Static) extends BallClock {
  }

  case class Runnable(stages: List[Accumulator.Tilting], end: Accumulator.Static.NonEmpty) extends BallClock {
    def countUntilCycle(): UIO[Duration] = {


      val schedule = (Schedule.unfoldM(this.tick)(_.tick).untilOutput(_ == this) && Schedule.forever)

      val result = ZIO.unit.repeat(schedule)
        .map { case (_, minutes) => Duration.ofMinutes(minutes) }

      result
    }


    /**
     * Perform all the motions related to one motion of the rotating arm (pick a ball,
     * @return
     */
    def tick: IO[Nothing, BallClock.Runnable] = {

      for {
        first <- end.takeFirst
      } yield {
        val (movingBall, initialNewEnd) = first
        val (newStages, newEnd) = BallClock.stageMotion(stages, Some(movingBall), initialNewEnd, Nil, Nil)
        this.copy(stages = newStages, end = newEnd)
      }
    }

  }



  def apply(): BallClock =
    NotRunnable(
      Accumulator.Tilting("Min", Nil, 5) ::
        Accumulator.Tilting("5Min", Nil, 12) ::
        Accumulator.Tilting("Hour", Nil, Ball(-1) :: Nil, 12) ::
      Nil,
      Accumulator.Static.Empty("Main")
    )
}