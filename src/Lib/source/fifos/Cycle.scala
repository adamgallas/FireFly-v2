package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object Cycle {
  def apply[T <: Data](streamIn: Stream[T], reuse: UInt, length: UInt, depth: Int, tech: String = "auto"): Stream[T] = {
    val fifo = new StreamCycleFifo(streamIn.payloadType, depth)
    if (tech != "auto")
      fifo.ram.addAttribute("ram_style", tech)
    fifo.io.push << streamIn
    fifo.io.reuse := reuse
    fifo.io.length := length
    fifo.io.pop
  }

//  def withOccupancy[T <: Data](streamIn: Stream[T], reuse: UInt, length: UInt, depth: Int, tech: String = "auto") = {
//    val fifo = new StreamCycleFifo(streamIn.payloadType, depth)
//    if (tech != "auto")
//      fifo.ram.addAttribute("ram_style", tech)
//    fifo.io.push << streamIn
//    fifo.io.reuse := reuse
//    fifo.io.length := length
//    (fifo.io.pop, fifo.io.occupancy)
//  }
}
