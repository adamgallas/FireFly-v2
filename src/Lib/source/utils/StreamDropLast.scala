package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamDropLast {
  def apply[T <: Data](input: Stream[Fragment[T]]): Stream[T] = {
    val output = Stream(input.dataType)
    output.arbitrationFrom(input)
    output.payload := input.fragment
    output
  }
}
