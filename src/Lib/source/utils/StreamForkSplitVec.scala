package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamForkSplitVec {
  def apply[T <: Data](in: Stream[Vec[T]], pipe: Boolean = false) = {
    val fork = StreamFork(in.toEvent(), 2)
    val up = GetHalfVec.up(in.payload)
    val down = GetHalfVec.down(in.payload)
    if (pipe)
      Vec(fork(0).translateWith(up).m2sPipe(), fork(1).translateWith(down).m2sPipe())
    else
      Vec(fork(0).translateWith(up), fork(1).translateWith(down))
  }

  def fragment[T <: Data](in: Stream[Fragment[Vec[T]]], pipe: Boolean = false) = {
    val fork = StreamFork(in.toEvent(), 2)
    val up = GetHalfVec.up(in.fragment)
    val down = GetHalfVec.down(in.fragment)
    if (pipe)
      Vec(fork(0).translateWith(up).addFragmentLast(in.last).m2sPipe(), fork(1).translateWith(down).addFragmentLast(in.last).m2sPipe())
    else
      Vec(fork(0).translateWith(up).addFragmentLast(in.last).m2sPipe(), fork(1).translateWith(down).addFragmentLast(in.last).m2sPipe())
  }
}
