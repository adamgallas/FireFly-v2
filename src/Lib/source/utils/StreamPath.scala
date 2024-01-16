package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamPath {
  def apply[TI <: Data, TO <: Data](
                                     input: Stream[TI], sel: Bool,
                                     path0: (Stream[TI], Stream[TO]),
                                     path1: (Stream[TI], Stream[TO])
                                   ) = {
    val path = StreamDemux(input, sel.asUInt, 2)
    path0._1 << path(0)
    path1._1 << path(1)
    StreamMux(sel.asUInt, Vec(path0._2, path1._2))
  }

  def apply[T <: Data](
                        input: Stream[T], action: Bool,
                        actionPath: (Stream[T], Stream[T])
                      ) = {
    val path = StreamDemux(input, action.asUInt, 2)
    actionPath._1 << path(1)
    StreamMux(action.asUInt, Vec(path(0), actionPath._2))
  }

  def apply[T <: Data](input: Stream[T], sel: Bool) = {
    val path = StreamDemux(input, sel.asUInt, 2)
    (path(0), path(1))
  }
}
