package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamForkSplitInnerVec {
  def apply[T <: Data](in: Stream[Vec[Vec[T]]]) = {
    val fork = StreamFork(in.toEvent(), 2)
    val up = GetInnerHalfVec.up(in.payload)
    val down = GetInnerHalfVec.down(in.payload)
    Vec(fork(0).translateWith(up), fork(1).translateWith(down))
  }

  def fragment[T <: Data](in: Stream[Fragment[Vec[Vec[T]]]]) = {
    val fork = StreamFork(in.toEvent(), 2)
    val up = GetInnerHalfVec.up(in.fragment)
    val down = GetInnerHalfVec.down(in.fragment)
    Vec(fork(0).translateWith(up).addFragmentLast(in.last), fork(1).translateWith(down).addFragmentLast(in.last))
  }
}
