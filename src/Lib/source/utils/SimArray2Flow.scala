package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object SimArray2Flow {
  def apply[T1 <: Data, T2](
                             flow: Flow[T1],
                             array: Array[T2],
                             clockDomain: ClockDomain,
                             driver: (T1, T2) => Unit,
                             alwaysValid: Boolean = false
                           ): Unit = {
    new SimArray2Flow(flow, array, clockDomain, driver, alwaysValid)
  }
}

class SimArray2Flow[T1 <: Data, T2](
                                     flow: Flow[T1],
                                     array: Array[T2],
                                     clockDomain: ClockDomain,
                                     driver: (T1, T2) => Unit,
                                     alwaysValid: Boolean = false
                                     ) {
  var index = 0
  flow.valid #= false

  while (index < array.length) {
    driver(flow.payload, array(index))

    if (alwaysValid) flow.valid #= true
    else flow.valid.randomize()

    clockDomain.waitSampling()
    if (flow.valid.toBoolean) {
      index = index + 1
    }
  }
  flow.valid #= false
}
