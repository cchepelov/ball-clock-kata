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


  val program: ZIO[Console with Clock, Any, Unit] = {
    ZIO.accessM[Clock with Console] { env =>
      for {
        time <- env.clock.currentDateTime
        ballClock <- BallClock().loadWithCurrentTime(time.toLocalTime, 27)

        refTimeClock <- Ref.make( (time, ballClock) )

        _ <- updateAndShow(refTimeClock).repeat(Schedule.linear(zio.duration.Duration.fromJava(Duration.ofSeconds(10))))
      } yield {
        ()
      }

    }



  }


  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    program.foldM(
      error => console.putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1)
      , _ => ZIO.succeed(0)
    )
}
