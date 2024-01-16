package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

object SimFlow2Array {
  def apply[T1 <: Data, T2](
                             flow: Flow[T1],
                             length: Int,
                             clockDomain: ClockDomain,
                             driver: T1 => T2
                           ) = {
    var index = 0
    val ret = ArrayBuffer[T2]()

    while (index < length) {
      clockDomain.waitSampling()
      if (flow.valid.toBoolean) {
        ret.append(driver(flow.payload))
        index = index + 1
      }
    }
    ret
  }
}
