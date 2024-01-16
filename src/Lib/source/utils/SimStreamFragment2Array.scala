package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object SimStreamFragment2Array {
  def apply[T1 <: Data, T2](
                             stream: Stream[Fragment[T1]],
                             array: Array[T2],
                             clockDomain: ClockDomain,
                             driver: (T1, T2) => Unit,
                             alwaysReady: Boolean = false
                           ): Unit = {
    new SimStreamFragment2Array(stream, array, clockDomain, driver, alwaysReady)
  }
}

class SimStreamFragment2Array[T1 <: Data, T2](
                                       stream: Stream[Fragment[T1]],
                                       array: Array[T2],
                                       clockDomain: ClockDomain,
                                       driver: (T1, T2) => Unit,
                                       alwaysReady: Boolean = false
                                     ) {
  var index = 0
  stream.ready #= false

  while (index < array.length) {

    if (alwaysReady) stream.ready #= true
    else stream.ready.randomize()

    clockDomain.waitSampling()
    if (stream.valid.toBoolean && stream.ready.toBoolean) {
      driver(stream.fragment, array(index))
      index = index + 1
    }
  }
  stream.ready #= false
}
