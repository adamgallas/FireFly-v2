package utils


import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object BitsMPerCh2BitsNPerCh {
  def apply(input: Stream[Bits], m:Int, n:Int) = {
    require(input.payload.getBitsWidth % m == 0)
    val payloadDiscard = Vec(input.payload.subdivideIn(m bits).map(_.dropHigh(m-n))).asBits
    input.translateWith(payloadDiscard)
  }
}
