package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object QueueAsync {
  def apply[T <: Data](streamIn: Stream[T], depth: Int): Stream[T] = {
    val fifo = new StreamFifoHighPerfAsync(streamIn.payloadType, depth)
    fifo.io.push << streamIn
    fifo.io.pop
  }
}