package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamHold {
  def apply[T <: Data](input: Stream[T], hold: Stream[UInt]) = {
    val join = StreamJoin(input, hold)
    val ret = Stream(input.payloadType)

    val cnt = Counter(hold.payload.getWidth - 1, ret.fire)
    val ovf = cnt === hold.payload - 1
    when(ovf && ret.fire)(cnt.clear())

    ret.valid := join.valid
    ret.payload := input.payload
    join.ready := ret.ready && ovf
    ret
  }
}
