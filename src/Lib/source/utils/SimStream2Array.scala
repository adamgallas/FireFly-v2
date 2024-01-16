package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

object SimStream2Array {
  def apply[T1 <: Data, T2](
                             stream: Stream[T1],
                             length: Int,
                             clockDomain: ClockDomain,
                             driver: T1 => T2,
                             alwaysReady: Boolean = false
                           ) = {
    var index = 0
    stream.ready #= false
    val ret = ArrayBuffer[T2]()

    while (index < length) {

      if (alwaysReady) stream.ready #= true
      else stream.ready.randomize()

      clockDomain.waitSampling()
      if (stream.valid.toBoolean && stream.ready.toBoolean) {
        ret.append(driver(stream.payload))
        index = index + 1
      }
    }
    stream.ready #= false
    ret
  }
}
