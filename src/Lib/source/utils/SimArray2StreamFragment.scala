package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object SimArray2StreamFragment {
  def apply[T1 <: Data, T2](
                             stream: Stream[Fragment[T1]],
                             array: Array[T2],
                             clockDomain: ClockDomain,
                             driver: (T1, T2) => Unit,
                             alwaysValid: Boolean = false
                           ): Unit = {
    new SimArray2StreamFragment(stream, array, clockDomain, driver, alwaysValid)
  }
}

class SimArray2StreamFragment[T1 <: Data, T2](
                                               stream: Stream[Fragment[T1]],
                                               array: Array[T2],
                                               clockDomain: ClockDomain,
                                               driver: (T1, T2) => Unit,
                                               alwaysValid: Boolean = false
                                             ) {
  var index = 0
  stream.valid #= false
  stream.last #= false

  while (index < array.length) {
    driver(stream.payload, array(index))

    if (alwaysValid) stream.valid #= true
    else stream.valid.randomize()

    if (index + 1 == array.length)
      stream.last #= true
    else
      stream.last #= false

    clockDomain.waitSampling()
    if (stream.valid.toBoolean && stream.ready.toBoolean) {
      index = index + 1
    }
  }
  stream.valid #= false
  stream.last #= false
}