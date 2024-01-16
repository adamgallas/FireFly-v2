package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamHaltWhenLast {
  def apply[T <: Data](input: Stream[Fragment[T]], haltCycles: Int) = {
    val halt = Bool() setAsReg() init false
    val cnt = Counter(haltCycles, inc = halt)
    halt.setWhen(input.fire && input.last).clearWhen(cnt.willOverflow)
    input.haltWhen(halt)
  }
}
