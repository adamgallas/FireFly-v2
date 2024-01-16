package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer


object SimStream2Queue {
  def apply[T1 <: Data, T2](
                             stream: Stream[T1],
                             clockDomain: ClockDomain,
                             driver: T1 => T2,
                             queueLimit: Int,
                             notFull: Int => Boolean,
                             enqueue: T2 => Unit
                           ) = {
    var index = 0
    stream.ready #= false

    while (true) {
      stream.ready #= notFull(queueLimit)
      clockDomain.waitSampling()
      if (stream.valid.toBoolean && stream.ready.toBoolean) {
        enqueue(driver(stream.payload))
        index = index + 1
      }
    }
    stream.ready #= false
  }
}
