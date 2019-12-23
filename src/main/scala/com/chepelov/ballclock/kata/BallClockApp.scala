package com.chepelov.ballclock.kata

import java.time.{Duration, OffsetDateTime}

import zio.clock.Clock
import zio.{App, IO, Ref, Schedule, UIO, ZEnv, ZIO, console}
import zio.console.Console

object BallClockApp extends App {


  private def showBallClock(ballClock: BallClock): ZIO[Console, Nothing, Any] =
    ZIO.accessM { env =>
      def showLine(accumulator: Accumulator): String =
        s"${accumulator.name.padTo(10, ' ')}: ${accumulator.contents.map(_.toString).mkString(" ")}"

      val tiltingLines = ballClock.stages.map(showLine)
      val staticLine = "          " + showLine(ballClock.end)

      ZIO.foreach(tiltingLines :+ staticLine) { line =>
        env.console.putStrLn(line)
      }
    }


  private def syncBallClockToClock(refTimeClock: Ref[(OffsetDateTime,BallClock.Runnable)]): ZIO[Clock, KataError, BallClock.Runnable] = {
    def syncUp(ct: OffsetDateTime, targetTime: OffsetDateTime, ballClock: BallClock.Runnable): ZIO[Clock, KataError, (OffsetDateTime, BallClock.Runnable)] = {

      for {
        minutes <- IO.effect { Duration.between(ct, targetTime).toMinutes.toInt }
          .mapError(t => KataError.UnderlyingError(t))

        newBallClock <- ballClock.runCycles(minutes)

        newTime <- IO.effect( ct.plusMinutes(minutes))
          .mapError(t => KataError.UnderlyingError(t))
      } yield {
        (newTime, newBallClock)
      }

    }

    ZIO.accessM[Clock] { env =>
      for {
        currentTime <- env.clock.currentDateTime

        rtc <- refTimeClock.get
        (lastKnownTime, lastBallClock) = rtc

        upRtc <- syncUp(lastKnownTime, currentTime, lastBallClock)
        (newKnownTime, newBallClock) = upRtc

        _ <- refTimeClock.set(newKnownTime, newBallClock)
      } yield {
        newBallClock
      }

    }
  }

  private def updateAndShow(refTimeClock: Ref[(OffsetDateTime, BallClock.Runnable)]): ZIO[Clock with Console, KataError, Unit] = {
    ZIO.accessM[Clock with Console] { env =>
      for {
        ballClock <- syncBallClockToClock(refTimeClock)
        _ <- env.console.putStrLn("\u001bc")
          _ <- showBallClock(ballClock)
      } yield ()

    }
  }


  def runInteractive(ballCount: Int): ZIO[Console with Clock, Any, Unit] = {
    ZIO.accessM[Clock with Console] { env =>
      for {
        time <- env.clock.currentDateTime
        ballClock <- BallClock().loadWithCurrentTime(time.toLocalTime, ballCount)

        refTimeClock <- Ref.make( (time, ballClock) )

        _ <- updateAndShow(refTimeClock).repeat(Schedule.linear(zio.duration.Duration.fromJava(Duration.ofSeconds(10))))
      } yield {
        ()
      }

    }
  }

  def cycleDuration(ballCount: Int): ZIO[Console, Any, Unit] = {
    ZIO.accessM[Console] { env =>
      for {
        ballclock <- BallClock().load(ballCount)
        duration <- ballclock.countUntilCycle()

        _ <- env.console.putStrLn(s"${ballCount} cycle after ${duration.toDays} days.  (precisely: ${duration})")
      } yield {
        ()
      }
    }
  }


  def cyclesAsJson(ballCount: Int, minutes: Int): ZIO[Console, Any, Unit] = {
    ZIO.accessM[Console] { env =>
      for {
        ballclock <- BallClock().load(ballCount)
        state <- ballclock.runCycles(minutes)

        _ <- env.console.putStrLn(s"${ballCount} cycle after ${minutes} minutes:")
        _ <- env.console.putStrLn(state.toString /* FIXME: as JSON !! */)
      } yield {
        ()
      }
    }
  }


  def program(args: List[String]): ZIO[Console with Clock, Any, Unit] =
    args match {
      case List("-r") | Nil => runInteractive(27)
      case List("-r", ballQty) => IO.effect(ballQty.toInt).map(runInteractive)

      case List(ballQtyStr) => for {
        ballQty <- IO.effect(ballQtyStr.toInt)
        _ <- cycleDuration(ballQty)
      } yield {
        ()
      }

      case List(ballQtyStr, minutesStr) => for {
        ballQty <- IO.effect(ballQtyStr.toInt)
        minutes <- IO.effect(minutesStr.toInt)

        _ <- cyclesAsJson(ballQty, minutes)
      } yield {
        ()
      }
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =

    program(args).foldM(
      error => console.putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1)
      , _ => ZIO.succeed(0)
    )
}
