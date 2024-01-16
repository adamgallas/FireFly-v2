package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object GetHalfVec {
  def up[T <: Data](in: Vec[T]) = {
    val length = in.length
    require(length % 2 == 0)
    Vec(in.splitAt(length / 2)._1)
  }

  def down[T <: Data](in: Vec[T]) = {
    val length = in.length
    require(length % 2 == 0)
    Vec(in.splitAt(length / 2)._2)
  }
}
