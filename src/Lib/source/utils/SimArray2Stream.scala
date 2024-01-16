package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object SimArray2Stream {
  def apply[T1 <: Data, T2](
                             stream: Stream[T1],
                             array: Array[T2],
                             clockDomain: ClockDomain,
                             driver: (T1, T2) => Unit,
                             alwaysValid: Boolean = false
                           ): Unit = {
    new SimArray2Stream(stream, array, clockDomain, driver, alwaysValid)
  }
}

class SimArray2Stream[T1 <: Data, T2](
                                       stream: Stream[T1],
                                       array: Array[T2],
                                       clockDomain: ClockDomain,
                                       driver: (T1, T2) => Unit,
                                       alwaysValid: Boolean = false
                                     ) {
  var index = 0
  stream.valid #= false

  while (index < array.length) {
    driver(stream.payload, array(index))

    if (alwaysValid) stream.valid #= true
    else stream.valid.randomize()

    clockDomain.waitSampling()
    if (stream.valid.toBoolean && stream.ready.toBoolean) {
      index = index + 1
    }
  }
  stream.valid #= false
}
