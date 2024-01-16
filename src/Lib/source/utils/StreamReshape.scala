package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamReshape {
  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: TO, n: Int) = {
    input.translateWith(Reshape(input.payload, outputType, n))
  }

  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: TO, n: (Int, Int)) = {
    input.translateWith(Reshape(input.payload, outputType, n))
  }

  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: TO, n: (Int, Int, Int)) = {
    input.translateWith(Reshape(input.payload, outputType, n))
  }

  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: TO, n: (Int, Int, Int, Int)) = {
    input.translateWith(Reshape(input.payload, outputType, n))
  }

  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: TO, n: (Int, Int, Int, Int, Int)) = {
    input.translateWith(Reshape(input.payload, outputType, n))
  }

}
