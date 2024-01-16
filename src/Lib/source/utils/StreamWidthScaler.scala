package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamWidthScaler {
  def apply[TI <: Data, TO <: Data](input: Stream[TI], outputType: HardType[TO]) = {
    val ret = Stream(outputType)
    StreamWidthAdapter(input, ret)
    ret
  }
}
