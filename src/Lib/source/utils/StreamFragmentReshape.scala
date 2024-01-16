package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamFragmentReshape {
  def apply[TI <: Data, TO <: Data](input: Stream[Fragment[TI]], outputType: TO, n: Int) = {
    input.translateWith(Reshape(input.fragment, outputType, n)).addFragmentLast(input.last)
  }

  def apply[TI <: Data, TO <: Data](input: Stream[Fragment[TI]], outputType: TO, n: (Int, Int)) = {
    input.translateWith(Reshape(input.fragment, outputType, n)).addFragmentLast(input.last)
  }

  def apply[TI <: Data, TO <: Data](input: Stream[Fragment[TI]], outputType: TO, n: (Int, Int, Int)) = {
    input.translateWith(Reshape(input.fragment, outputType, n)).addFragmentLast(input.last)
  }

  def apply[TI <: Data, TO <: Data](input: Stream[Fragment[TI]], outputType: TO, n: (Int, Int, Int, Int)) = {
    input.translateWith(Reshape(input.fragment, outputType, n)).addFragmentLast(input.last)
  }

  def apply[TI <: Data, TO <: Data](input: Stream[Fragment[TI]], outputType: TO, n: (Int, Int, Int, Int, Int)) = {
    input.translateWith(Reshape(input.fragment, outputType, n)).addFragmentLast(input.last)
  }

}
