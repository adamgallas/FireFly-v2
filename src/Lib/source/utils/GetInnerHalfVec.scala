package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object GetInnerHalfVec {
  def up[T <: Data](in: Vec[Vec[T]]) = {
    val length = in.head.length
    require(length % 2 == 0)
    Vec(in.map(v => Vec(v.splitAt(length / 2)._1)))
  }

  def down[T <: Data](in: Vec[Vec[T]]) = {
    val length = in.head.length
    require(length % 2 == 0)
    Vec(in.map(v => Vec(v.splitAt(length / 2)._2)))
  }
}
