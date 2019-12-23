package com.chepelov.ballclock.kata

import java.time.Duration

import org.scalacheck.Gen
import org.scalatest.Assertions
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.{AnyFlatSpec, AsyncFlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import zio._

class BallClockSpec extends  AsyncFlatSpec with  Matchers with ZioTestAdapter with ZEnvDefinition
  with ScalaCheckDrivenPropertyChecks{

  implicit val zioRuntime: zio.Runtime[ZEnv] = new DefaultRuntime { }

  behavior of "empty BallClock"

  it should "reject loading with 0 balls" in {
    zTest {
      BallClock().load(0)
          .flip
          .map( _ shouldBe a[KataError.InsufficientBallCount])
    }
  }

  it should "reject loading with any insufficient amount of balls" in
    forAll(Gen.choose(0, (5+11+11) - 1)) { quantity =>
      zTestSync {
        BallClock().load(quantity)
          .flip
          .map( _ shouldBe a[KataError.InsufficientBallCount])
      }
    }

  it should "accept the minimum quantity of balls" in {
    zTest {
      for {
        clock <- BallClock().load(27)
      } yield {
        clock.stages.foreach(stage => stage.moving shouldBe empty)
        clock.end.size shouldEqual (27 + 1) // 1 extra is the glued ball on the Hour display
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
        clock.end.size shouldEqual (quantity + 1) // 1 extra is the glued ball on the Hour display
      }
    }
  }

    it should "reject loading with any excessive amoount of balls" in
      forAll(Gen.choose(128, Int.MaxValue)) { quantity =>
        zTestSync {
          BallClock().load(quantity)
            .flip
            .map( _ shouldBe a[KataError.ExcessiveBallCount])
        }
      }


  behavior of "loaded BallClock"

  "BallClock loaded with 30 balls" should "cycle after 15 days" in zTest {
    for {
      clock <- BallClock().load(30)
      duration <- clock.countUntilCycle()
    } yield {
      duration shouldBe >= (Duration.ofDays(15))
      duration shouldBe < (Duration.ofDays(16))
    }
  }

  "BallClock loaded with 45 balls" should "cycle after 378 days" in zTest {
    for {
      clock <- BallClock().load(45)
      duration <- clock.countUntilCycle()
    } yield {
      duration shouldBe >= (Duration.ofDays(378))
      duration shouldBe < (Duration.ofDays(379))
    }
  }





}
