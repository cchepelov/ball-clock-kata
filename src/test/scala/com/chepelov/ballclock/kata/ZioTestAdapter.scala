package com.chepelov.ballclock.kata

import org.scalatest.compatible
import zio.ZIO

import scala.concurrent.Future
import scala.language.implicitConversions

object ZioTestAdapterTools {
  case class ZioErrorAdapter[R, E <: Throwable, A](io: ZIO[R, E, A]) {
    def adaptedIO: ZIO[R, E, A] = io
  }
  case class ZioErrorChannelNonEmpty[E](error: E) extends Exception(s"Checked error not handled: ${error}")
}

trait ZioTestAdapterToolsLowPrio {
  import ZioTestAdapterTools._
  implicit def adaptExceptionChannel[R, E, A](io: ZIO[R, E, A]): ZioErrorAdapter[R, ZioErrorChannelNonEmpty[E], A] =
    ZioErrorAdapter(io.mapError(ZioErrorChannelNonEmpty(_)))
}
object ZioTestAdapterToolsImplicits extends ZioTestAdapterToolsLowPrio {
  import ZioTestAdapterTools._

  implicit def nopAdapter[R, E <: Throwable, A](io: ZIO[R, E, A]): ZioErrorAdapter[R, E, A] =
    ZioErrorAdapter(io)
}

trait ZioTestAdapter {
  def zTest[R, E](io: ZIO[R, E, compatible.Assertion])
                 (implicit zioRuntime: zio.Runtime[R]): Future[compatible.Assertion] = {
    import ZioTestAdapterTools._
    import ZioTestAdapterToolsImplicits._

    val adapted: ZioErrorAdapter[R, _ <: Throwable, compatible.Assertion] = io // selects nopAdapter or adaptExceptionChannel based on E
    zioRuntime.unsafeRunToFuture(adapted.adaptedIO)
  }


  def zTestSync[R, E](io: ZIO[R, E, compatible.Assertion])
                     (implicit zioRuntime: zio.Runtime[R]): compatible.Assertion = {
    import ZioTestAdapterTools._
    import ZioTestAdapterToolsImplicits._

    val adapted: ZioErrorAdapter[R, _ <: Throwable, compatible.Assertion] = io // selects nopAdapter or adaptExceptionChannel based on E
    zioRuntime.unsafeRun(adapted.adaptedIO)
  }
}