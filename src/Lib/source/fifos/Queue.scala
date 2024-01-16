package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object Queue {
  def apply[T <: Data](streamIn: Stream[T], depth: Int, tech: String = "auto"): Stream[T] = {
    val fifo = new StreamFifoHighPerf(streamIn.payloadType, depth)
    if (tech != "auto")
      fifo.ram.addAttribute("ram_style", tech)
    fifo.io.push << streamIn
    fifo.io.pop
  }
}
