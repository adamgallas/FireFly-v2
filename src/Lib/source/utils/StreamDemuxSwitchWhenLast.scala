package utils

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

object StreamDemuxSwitchWhenLast {
  def apply[T <: Data](input: Stream[Fragment[T]], channels: Int) = {
    val paramSel = Counter(channels, inc = input.fire && input.last)
    StreamDemux(input, paramSel, channels)
  }
}
