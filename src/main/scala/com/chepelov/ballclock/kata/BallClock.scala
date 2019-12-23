package com.chepelov.ballclock.kata

import java.time.{Duration, LocalTime, OffsetDateTime, ZoneOffset}

import zio.{IO, Schedule, UIO, ZIO}
import cats.Foldable
import cats.instances.order
import cats.implicits._
import cats.Monoid
import com.chepelov.ballclock.kata
import play.api.libs.json
import play.api.libs.json.{JsArray, JsObject, JsValue, Writes}
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
    validateSizes(ballCount) {
      val newBalls = makeBalls(ballCount)

      for {
        newEnd <- BallClock.feedBallsIntoMain(end, newBalls)
      } yield {
        BallClock.Runnable(stages = this.stages, end = newEnd)
      }
    }
  }

  private def makeBalls(ballCount: Int) = {
    val maxExistingBall = (end :: stages).flatMap(_.contents.to[List].maximumOption).maximumOption

    val firstBallId = maxExistingBall.map(_.id).filterNot(_ < 0).getOrElse(0) // we locate the first ID of the first non-static ball
    val newBalls = (0 until ballCount).map(num => Ball(firstBallId + 1 + num))
    newBalls.to[List]
  }

  private def validateSizes(ballCount: Int)(innerOp: => IO[KataError, BallClock.Runnable]): IO[KataError, BallClock.Runnable] = {
    import BallClock.MaxCapacity

    val (existingCapacity, existingSize, staticSize) =
      Monoid[(Int, Int, Int)].combineAll(
        (1, 0, 0) ::  stages.map(st => (st.capacity - st.static.size - 1, st.size - st.static.size, st.static.size)))

    if ((existingSize + ballCount) < existingCapacity) {
      IO.fail(KataError.InsufficientBallCount(ballCount, existingCapacity, existingSize))
    } else if ((existingSize + ballCount) > MaxCapacity) {
      IO.fail(KataError.ExcessiveBallCount(ballCount, existingCapacity, existingSize, MaxCapacity + staticSize))
    } else {
      innerOp
    }
  }


  def loadWithMidnight(ballCount: Int): IO[KataError, BallClock.Runnable] = {
    val time = LocalTime.of(0, 0)
    loadWithCurrentTime(time, ballCount)
  }

  def loadWithCurrentTime(time: LocalTime, ballCount: Int): IO[KataError, BallClock.Runnable] = {
    validateSizes(ballCount) {

      val minutesOfHour = time.getMinute
      val hourOfDay = time.getHour

      val minutesInDay = minutesOfHour + (60 * hourOfDay)

      val parceledOut = stages.scanLeft( (List.empty[Ball], makeBalls(ballCount), minutesInDay)) { case ((_, balls, counter), stage) =>
        val toTake = stage.capacity - stage.static.size

        val desiredValue = counter % toTake

        val splitPos = {
          val rawpos = (counter - stage.static.size) % toTake
          if (rawpos < 0) {
            rawpos + toTake
          }  else {
            rawpos
          }
        }
        val (takenBalls, leftBalls) = balls.splitAt( splitPos )
        val newCounter = (counter - stage.static.size) / toTake

        (takenBalls, leftBalls, newCounter)
      }
        .tail // the first record is just the start, not useful.

      val filledStages = stages.zip(parceledOut).map { case (stage, (taken, _, _)) => stage.copy(moving = taken.reverse) }

      val (_, finalBalls, _) = parceledOut.last


      for {
        newEnd <- BallClock.feedBallsIntoMain(end, finalBalls)
      } yield {
        BallClock.Runnable(stages = filledStages, end = newEnd)
      }
    }
  }
}


object BallClock {

  private def feedBallsIntoMain(end: Accumulator.Static, toReceive: List[Ball]): IO[KataError.Empty.type, Accumulator.Static.NonEmpty] =
    end match {
      case nonEmptyEnd: Accumulator.Static.NonEmpty =>
        UIO {
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
        }
      case emptyEnd: Accumulator.Static.Empty =>
        toReceive match {
          case Nil =>
            IO.fail(KataError.Empty)

          case singleBall :: Nil =>
            val (newEndState, None) = emptyEnd.receive(singleBall)
            UIO(newEndState)

          case firstBall :: otherBalls =>
            val (firstNewEndState, None) = emptyEnd.receive(firstBall)

            val result = otherBalls.foldLeft(firstNewEndState) { case (endState, ball) =>
              val (newEndState, None) = endState.receive(ball)
              newEndState
            }

            UIO(result)
        }
    }



  @tailrec
  private def stageMotion(stages: List[Accumulator.Tilting], maybeBall: Option[Ball],
                          end: Accumulator.Static,
                          towardsEnd: Seq[Ball] = Nil, newStagesReversed: List[Accumulator.Tilting]): IO[KataError, (List[Accumulator.Tilting], Accumulator.Static.NonEmpty)] = {
    stages match {
      case Nil =>
        // everything that might be moving, is moving. Time to collect every ball hurtling towards
        // the static accumulator. ENDING with the ball moving from the last tilt, if any, as this is what the text
        // specifies.

        for {
          tempEnd <- feedBallsIntoMain(end, towardsEnd.to[List])
          newEnd <- maybeBall match {
            case None =>
              UIO(tempEnd)
            case Some(ball) =>
              feedBallsIntoMain(tempEnd, ball :: Nil) // the ball that started it all
          }
        } yield {
          (newStagesReversed.reverse, newEnd)
        }

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
                  newCurrentStage :: newStagesReversed)
            }
        }
    }
  }

  val MaxCapacity: Int = 127

  private case class NotRunnable(stages: List[Accumulator.Tilting], end: Accumulator.Static) extends BallClock {
  }

  private def runUntilLikeStartOrError[R, E, A](start: A, stepIO: A => ZIO[R, E, A], stopCondition: A => Boolean) =
    Schedule
      .unfoldM(stepIO(start).traced.either) {
        case error @ Left(_) => UIO(error)
        case Right(state) => stepIO(state).either
      }
      .untilOutput {
        case Left(_) =>
          true
        case Right(state) =>
          if (stopCondition(state)) {
            true
          } else false

      }


  case class Runnable(stages: List[Accumulator.Tilting], end: Accumulator.Static.NonEmpty) extends BallClock {
    def runCycles(minutes: Int): IO[KataError, BallClock.Runnable] = {
      if (minutes == 0) {
        UIO(this)
      } else if (minutes == 1) {
        this.tick
      } else {
        val schedule = {
          def nextTick(r: BallClock.Runnable, count: Int) = r.tick.map(nr => nr -> (count + 1))
          def stopCond(r: BallClock.Runnable, count: Int) = count == minutes

          runUntilLikeStartOrError[Any, KataError, (BallClock.Runnable, Int)]((this, 0),
            _ match { case (r, count) => nextTick(r, count) },
            _ match { case (r, count) => stopCond(r, count) })
        }

        val result = for {
          repetitionResult <- ZIO.unit.repeat(schedule)
          resultItem <- IO.fromEither(repetitionResult)

        } yield {
          val (resultBallClock, _) = resultItem
          resultBallClock
        }
        result
      }
    }

    def countUntilCycle(): IO[KataError, Duration] = {


      val schedule = {
        def nextTick(r: BallClock.Runnable, count: Int) = r.tick.map(nr => nr -> (count + 1))
        def stopCond(r: BallClock.Runnable, count: Int) = this == r

        runUntilLikeStartOrError[Any, KataError, (BallClock.Runnable, Int)]((this, 0),
          _ match { case (r, count) => nextTick(r, count) },
          _ match { case (r, count) => stopCond(r, count) })
      }


      val result = ZIO.unit.repeat(schedule)
        .flatMap {
          case (Left(error)) => IO.fail(error)
          case (Right((_, minutes))) => IO.succeed(Duration.ofMinutes(minutes + 1))
        }

      result
    }


    /**
     * Perform all the motions related to one motion of the rotating arm (pick a ball,
     * @return
     */
    def tick: IO[KataError, BallClock.Runnable] = {

      for {
        first <- end.takeFirst
        (movingBall, initialNewEnd) = first

        motion <- BallClock.stageMotion(stages, Some(movingBall), initialNewEnd, Nil, Nil)
        (newStages, newEnd) = motion
      } yield {
        this.copy(stages = newStages, end = newEnd)
      }
    }
  }
  object Runnable {
    implicit object WritesRunnable extends Writes[Runnable] {
      override def writes(o: Runnable): JsValue = {
        val stageJson = (o.stages.map(stage => stage.name -> stage.moving.reverse) :+
          (o.end.name -> o.end.contents.reverse.to[List]))
            .map {
              case (name, balls) => name -> JsArray(balls.map(implicitly[Writes[Ball]].writes))
            }

        JsObject(stageJson)
      }
    }
  }



  def apply(): BallClock =
    NotRunnable(
      Accumulator.Tilting("Min", Nil, 5) ::
        Accumulator.Tilting("5Min", Nil, 12) ::
        Accumulator.Tilting("Hour", Nil, Ball(-1) :: Nil, 13) ::
      Nil,
      Accumulator.Static.Empty("Main")
    )


  implicit object WritesBallClock extends Writes[BallClock] {
    override def writes(o: BallClock): JsValue = o match {
      case r: BallClock.Runnable =>
        implicitly[Writes[BallClock.Runnable]].writes(r)

      case r: BallClock.NotRunnable =>
        ???
    }
  }
}