package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamAddLast {
  def apply[T <: Data](source: Stream[T], bound: UInt) = {
    val ret = Stream(Fragment(source.payloadType))
    val cnt = Counter(bound.getWidth bits, inc = source.fire)
    val reached = cnt === bound
    when(cnt.willIncrement && reached)(cnt.clear())
    ret.arbitrationFrom(source)
    ret.fragment := source.payload
    ret.last := reached
    ret
  }
}
