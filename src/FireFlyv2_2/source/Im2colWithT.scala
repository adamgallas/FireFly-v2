import fifos.StreamFifoHighPerf
import im2col.{Im2colPushPopLogic, StreamTimingImprove, TensorPadding}
import spinal.core._
import spinal.lib._
import utils.MultiPortRam

import scala.language.postfixOps

class Im2colWithT[T <: Data](dataType: HardType[T], cp: Int, pp: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val addrWidth = log2Up(bufferDepth)
  val io = new Bundle {
    val push = slave(Stream(Vec(dataType, cp)))
    val pop = master(Stream(Vec(Vec(dataType, cp), pp)))
  }

  val pad4Conv = new TensorPadding(Vec(dataType, cp), List(8, 10, 10))
  val pushPop = new Im2colWithTPushPopLogic(cp, pp, bufferDepth)
  val cache = new MultiPortRam(Vec(dataType, cp), pp, bufferDepth, tech)

  val cfgPad4conv = pad4Conv.config.toIo()
  val cfgIm2col = pushPop.cfg.toIo()

  io.push >> pad4Conv.io.push
  val paddedStream = pad4Conv.io.pop.s2mPipe().m2sPipe()

  val wrEvent = paddedStream.toEvent()
  val wrCmd = Flow(utils.Linked(UInt(addrWidth bits), Vec(dataType, cp)))
  pushPop.io.pushEvent << wrEvent
  wrCmd.valid := pushPop.io.pushEvent.fire
  wrCmd.linked := paddedStream.payload
  wrCmd.value := pushPop.io.pushPtr
  cache.io.wrCmd << wrCmd

  val rdEvent = pushPop.io.popEvent
  val rdAddr = pushPop.io.popPtr
  val rdValid = pushPop.io.popEvent.fire
  val rdCmds = Flow(Vec(UInt(addrWidth bits), pp))
  rdCmds.valid := rdValid
  rdCmds.payload := rdAddr
  cache.io.rdCmds << rdCmds

  val fifo = new StreamFifoHighPerf(Vec(Vec(dataType, cp), pp), 8)
  fifo.io.push.valid := cache.io.rdData.valid
  fifo.io.push.payload := cache.io.rdData.payload
  io.pop << fifo.io.pop
  rdEvent.ready := io.pop.ready

  //  val expose = pushPop.expose.toIo()
}

object Im2colWithT {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2colWithT(Bits(4 bits), 4, 16, 2048 * 16, "block"))
  }
}
