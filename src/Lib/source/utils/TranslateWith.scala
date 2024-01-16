package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object TranslateWith {
  def apply[TI <: Data, TO <: Data](stream: Stream[Fragment[TI]], payload: TO) = {
    val ret = new Stream(Fragment(payload))
    ret.arbitrationFrom(stream)
    ret.last := stream.last
    ret.fragment := payload
    ret
  }
}
