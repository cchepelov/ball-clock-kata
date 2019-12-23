package com.chepelov.ballclock.kata

import java.time.{Duration, LocalTime, OffsetDateTime, ZoneOffset}

import org.scalacheck.Gen
import org.scalatest.Assertions
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.{AnyFlatSpec, AsyncFlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import zio._

class BallClockSpec extends  AsyncFlatSpec with  Matchers with ZioTestAdapter with ZEnvDefinition
  with ScalaCheckDrivenPropertyChecks {

  implicit val zioRuntime: zio.Runtime[ZEnv] = new DefaultRuntime {}

  behavior of "empty BallClock"

  it should "reject loading with 0 balls" in {
    zTest {
      BallClock().load(0)
        .flip
        .map(_ shouldBe a[KataError.InsufficientBallCount])
    }
  }

  it should "reject loading with any insufficient amount of balls" in
    forAll(Gen.choose(0, (5 + 11 + 11) - 1)) { quantity =>
      zTestSync {
        BallClock().load(quantity)
          .flip
          .map(_ shouldBe a[KataError.InsufficientBallCount])
      }
    }

  it should "accept the minimum quantity of balls" in {
    zTest {
      for {
        clock <- BallClock().load(27)
      } yield {
        clock.stages.foreach(stage => stage.moving shouldBe empty)
        clock.end.size shouldEqual (27)
      }
    }
  }

  it should "accept any valid quantity of balls (27 to 127 inclusive)" in
    forAll(Gen.chooseNum(27, 127)) { quantity =>
      zTestSync {
        for {
          clock <- BallClock().load(quantity)
        } yield {
          clock.stages.foreach(stage => stage.moving shouldBe empty)
          clock.end.size shouldEqual (quantity)
        }
      }
    }

  it should "reject loading with any excessive amoount of balls" in
    forAll(Gen.choose(128, Int.MaxValue)) { quantity =>
      zTestSync {
        BallClock().load(quantity)
          .flip
          .map(_ shouldBe a[KataError.ExcessiveBallCount])
      }
    }

  it should "show the expected state 0 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock = clock
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents shouldBe empty

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        30 :: 29 :: 28 :: 27 :: 26 :: 25 :: 24 :: 23 :: 22 :: 21 ::
          20 :: 19 :: 18 :: 17 :: 16 :: 15 :: 14 :: 13 :: 12 :: 11 ::
          10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: Nil)
    }
  }

  it should "show the expected state 4 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(4)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) should contain theSameElementsInOrderAs (4 :: 3 :: 2 :: 1 :: Nil)

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents shouldBe empty

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        30 :: 29 :: 28 :: 27 :: 26 :: 25 :: 24 :: 23 :: 22 :: 21 ::
          20 :: 19 :: 18 :: 17 :: 16 :: 15 :: 14 :: 13 :: 12 :: 11 ::
          10 :: 9 :: 8 :: 7 :: 6 :: 5 :: Nil)
    }
  }

  it should "show the expected state 5 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(5)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs (5 :: Nil)

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        1 :: 2 :: 3 :: 4 :: 30 :: 29 :: 28 :: 27 :: 26 :: 25 :: 24 :: 23 :: 22 :: 21 ::
          20 :: 19 :: 18 :: 17 :: 16 :: 15 :: 14 :: 13 :: 12 :: 11 ::
          10 :: 9 :: 8 :: 7 :: 6 :: Nil)
    }
  }

  it should "show the expected state 10 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(10)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs (10 :: 5 :: Nil)

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        6 :: 7 :: 8 :: 9 :: 1 :: 2 :: 3 :: 4 :: 30 :: 29 :: 28 :: 27 :: 26 :: 25 :: 24 :: 23 :: 22 :: 21 ::
          20 :: 19 :: 18 :: 17 :: 16 :: 15 :: 14 :: 13 :: 12 :: 11 ::  Nil)
    }
  }

  it should "show the expected state 30 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(30)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs (30 :: 25 :: 20 :: 15 :: 10 :: 5 :: Nil)

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
      26 :: 27 :: 28 :: 29 ::
       21 :: 22 :: 23 :: 24 ::
        16 :: 17 :: 18 :: 19 ::
        11 :: 12 :: 13 :: 14 ::
         6 :: 7 :: 8 :: 9 ::
         1 :: 2 :: 3 :: 4 :: Nil)
    }
  }

  it should "show the expected state 35 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(35)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs (9 :: 30 :: 25 :: 20 :: 15 :: 10 :: 5 :: Nil)

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        4 :: 3 :: 2 :: 1 ::
        26 :: 27 :: 28 :: 29 ::
          21 :: 22 :: 23 :: 24 ::
          16 :: 17 :: 18 :: 19 ::
          11 :: 12 :: 13 :: 14 ::
          6 :: 7 :: 8 :: Nil)
    }
  }

  it should "show the expected state 59 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(59)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) should contain theSameElementsInOrderAs (14 :: 4 :: 3 :: 2 :: Nil)

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents should have size 11
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs (1 :: 21 :: 17 :: 13 :: 9 :: 30 :: 25 :: 20 :: 15 :: 10 :: 5 :: Nil)

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(-1) :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        29 :: 28 :: 27 :: 26 :: 16 :: 24 :: 23 :: 22 ::
          12 :: 11 :: 19 :: 18 ::
          8 :: 7 :: 6 :: Nil)
    }
  }

  it should "show the expected state 60 minutes after the zero starting-point with 30 marbles" in zTest {
    for {
      clock <- BallClock().load(30)
      resultClock <- clock.runCycles(60)
    } yield {
      val minutes = resultClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = resultClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) shouldBe empty

      val hours = resultClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents.map(_.id) should contain theSameElementsInOrderAs (6 :: -1 :: Nil)

      resultClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        5 :: 10 :: 15 :: 20 :: 25 :: 30 :: 9 :: 13 :: 17 :: 21 :: 1 ::
        2 :: 3 :: 4 :: 14 ::
        29 :: 28 :: 27 :: 26 :: 16 :: 24 :: 23 :: 22 ::
          12 :: 11 :: 19 :: 18 ::
          8 :: 7 :: Nil)
    }
  }

  it should "show the expected state when starting at 12:59 with 30 marbles" in zTest {
    for {
      ballClock <- BallClock().loadWithCurrentTime(LocalTime.of(12, 59), 30)
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) should contain theSameElementsInOrderAs(4 :: 3 :: 2 :: 1 :: Nil)

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs(15 :: 14 :: 13 :: 12 :: 11 :: 10 :: 9 :: 8 :: 7 :: 6 :: 5 :: Nil)

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should have size 12
      hours.contents.map(_.id) should contain theSameElementsInOrderAs ( 26 :: 25 :: 24 :: 23 :: 22 :: 21 :: 20 :: 19 :: 18 :: 17 :: 16 :: -1 :: Nil)

      ballClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        30 :: 29 :: 28 :: 27 :: Nil)

    }
  }

  it should "tilt towars the expected state when starting at 12:59 with 30 marbles" in zTest {
    for {
      startingBallClock <- BallClock().loadWithCurrentTime(LocalTime.of(12, 59), 30)
      ballClock <- startingBallClock.tick
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) shouldBe empty

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) shouldBe empty

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should have size 1
      hours.contents.map(_.id) should contain theSameElementsInOrderAs ( -1 :: Nil)

      /* this test is critical: it proves the correct ordering in once-every-12-hours events.
      *
      * Here the ball that "itself also returns to the queue" at the end of the text is #27 */

      ballClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        27 ::
        16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: 24 :: 25 :: 26 ::
        5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 ::
        1 :: 2 :: 3 :: 4 ::
        30 :: 29 :: 28 :: Nil)

    }
  }

  it should "show the expected state 325 minutes after the zero starting-point with 30 marbles" ignore zTest {
    /* this test was picked up from the JSON clause; not sure whether the data shown was actually meant to be correct */

    for {
      clock <- BallClock().load(30)
      clock325 <- clock.runCycles(325)
    } yield {
      val minutes = clock325.stages.head
      minutes.static shouldBe empty
      minutes.contents shouldBe empty

      val fiveMins = clock325.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents should contain theSameElementsInOrderAs (
        Ball(7) :: Ball(3) :: Ball(25) :: Ball(13) :: Ball(22) :: Nil)

      val hours = clock325.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(15) :: Ball(4) :: Ball(17) :: Ball(12) :: Ball(6) :: Ball(-1) :: Nil)

      clock325.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        11 :: 5 :: 26 :: 18 :: 2 :: 30 :: 19 :: 8 :: 24 :: 10 :: 29 :: 20 :: 16 :: 21 :: 28 :: 1 :: 23 :: 14 :: 27 :: 9 :: Nil
        )
    }
  }

  behavior of "loaded BallClock"

  "BallClock loaded with 30 balls" should "cycle after 15 days" in zTest {
    for {
      clock <- BallClock().load(30)
      rawDuration <- clock.countUntilCycle()
      duration = rawDuration.plusMinutes(1)
    } yield {
      duration shouldBe >= (Duration.ofDays(15))
      duration shouldBe <(Duration.ofDays(16))
    }
  }

  "BallClock loaded with 45 balls" should "cycle after 378 days" in zTest {
    for {
      clock <- BallClock().load(45)
      rawDuration <- clock.countUntilCycle()
      duration = rawDuration.plusMinutes(1)
    } yield {
      duration shouldBe >=(Duration.ofDays(378))
      duration shouldBe <(Duration.ofDays(379))
    }
  }

  "A BallClock(30) loaded at midnight" should "look like an expected situation" in zTest {
    for {
      ballClock <- BallClock().loadWithMidnight(30)
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents shouldBe empty

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents shouldBe empty

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should have size 12
      hours.contents.map(_.id) should contain theSameElementsInOrderAs ( 11 :: 10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: -1 :: Nil)

      ballClock.end.contents.map(_.id) should contain theSameElementsInOrderAs (
        30 :: 29 :: 28 :: 27 :: 26 :: 25 :: 24 :: 23 :: 22 :: 21 ::
        20 :: 19 :: 18 :: 17 :: 16 :: 15 :: 14 :: 13 :: 12 :: Nil)

    }

  }

  "At 15:55, BallClock" should "look like an expected situation" in zTest {
    val time = LocalTime.of(15, 55)

    for {
      ballClock <- BallClock().loadWithCurrentTime(time, 27)
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents should contain theSameElementsInOrderAs ( Nil)

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents should contain theSameElementsInOrderAs (
        Ball(11) :: Ball(10) :: Ball(9) ::
        Ball(8) :: Ball(7) :: Ball(6) :: Ball(5) :: Ball(4) :: Ball(3) :: Ball(2) :: Ball(1) :: Nil)

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(13) :: Ball(12) :: Ball(-1) :: Nil)
    }
  }

  it should "tilt towards an expected situation once the time is right" in zTest {
    val time = LocalTime.of(15, 55)

    for {
      prevBallClock <- BallClock().loadWithCurrentTime(time, 27)
      ballClock <- prevBallClock.tick
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents.map(_.id) should contain theSameElementsInOrderAs ( 14 :: Nil)

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents.map(_.id) should contain theSameElementsInOrderAs ( 11 :: 10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: Nil)

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents.map(_.id) should contain theSameElementsInOrderAs (13 :: 12 :: -1 :: Nil)
    }
  }


  "At 15:59, BallClock" should "look like an expected situation" in zTest {
    val time = LocalTime.of(15, 59)

    for {
      ballClock <- BallClock().loadWithCurrentTime(time, 27)
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents should contain theSameElementsInOrderAs (Ball(4) :: Ball(3) :: Ball(2) :: Ball(1) :: Nil)

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents should contain theSameElementsInOrderAs (
        /* TILT! */ Ball(15) :: Ball(14) :: Ball(13) ::
        Ball(12) :: Ball(11) :: Ball(10) :: Ball(9) ::
        Ball(8) :: Ball(7) :: Ball(6) :: Ball(5) :: Nil)

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(17) :: Ball(16) :: Ball(-1) :: Nil)
    }
  }

  it should "tilt towards an expected situation once the time is right" in zTest {
    val time = LocalTime.of(15, 59)

    for {
      prevBallClock <- BallClock().loadWithCurrentTime(time, 27)
      ballClock <- prevBallClock.tick
    } yield {
      val minutes = ballClock.stages.head
      minutes.static shouldBe empty
      minutes.contents shouldBe empty

      val fiveMins = ballClock.stages(1)
      fiveMins.static shouldBe empty
      fiveMins.contents shouldBe empty

      val hours = ballClock.stages(2)
      hours.static should contain theSameElementsInOrderAs (Ball(-1) :: Nil)
      hours.contents should contain theSameElementsInOrderAs (Ball(18) :: Ball(17) :: Ball(16) :: Ball(-1) :: Nil)
    }
  }


}
