package im2col

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._
import utils.MultiPortRam

import scala.language.postfixOps

class Im2col[T <: Data](dataType: HardType[T], cp: Int, pp: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val addrWidth = log2Up(bufferDepth)
  val io = new Bundle {
    val push = slave(Stream(Vec(dataType, cp)))
    val pop = master(Stream(Vec(Vec(dataType, cp), pp)))
  }

  val pad4Conv = new TensorPadding(Vec(dataType, cp), List(10 - log2Up(cp), 10, 10))
  val pad4Coalesce = new TensorPadding(Vec(dataType, cp), List(10))
  val timing = new StreamTimingImprove(Vec(dataType, cp))
  val pushPop = new Im2colPushPopLogic2(cp, pp, bufferDepth)
  val cache = new MultiPortRam(Vec(dataType, cp), pp, bufferDepth, tech)

  val cfgPad4conv = pad4Conv.config.toIo()
  val cfgPad4coalesce = pad4Coalesce.config.toIo()
  val cfgPush = pushPop.pushConfig.toIo()
  val cfgPop = pushPop.popConfig.toIo()

  io.push >> pad4Conv.io.push
  pad4Conv.io.pop >> pad4Coalesce.io.push
  pad4Coalesce.io.pop >> timing.io.push

  val paddedStream = timing.io.pop

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
}

object Im2col {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2col(Bits(4 bits), 16, 8, 8192,"ultra"))
  }
}
