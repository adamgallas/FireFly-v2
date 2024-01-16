package utils

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class StartingPointCache[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  val io = new Bundle {
    val enableIn = in Bool()
    val enableOut = in Bool()
    val input = in(dataType())
    val output = out(dataType())
    val outputNext = out(dataType())
  }
  val fifo = StreamFifoHighPerf(dataType, depth)
  fifo.io.push.valid := io.enableIn
  fifo.io.pop.ready := io.enableOut
  fifo.io.push.payload := io.input

  val startPoint = dataType() setAsReg() init dataType().getZero
  val startPointNext = dataType()
  startPoint := startPointNext
  startPointNext := startPoint

  when(io.enableOut) {
    startPointNext := fifo.io.pop.payload
  }

  io.output := startPoint
  io.outputNext := startPointNext
}
