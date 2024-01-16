package systolic

import spinal.lib._
import spinal.core._
import fifos._

import scala.language.postfixOps

case class MemReadPortArbiter[T <: Data](dataType: HardType[T], channels: Int, addrWidth: Int) extends Component {
  val io = new Bundle {
    val addrFromCmd = Vec(slave(Stream(UInt(addrWidth bits))), channels)
    val addrToMem = Vec(master(Flow(UInt(addrWidth - log2Up(channels) bits))), channels)
    val dataFromMem = Vec(slave(Flow(dataType)), channels)
    val dataToCmd = Vec(master(Stream(dataType)), channels)
  }
  noIoPrefix()

  val addrFromCmdDemux = io.addrFromCmd.map(s =>
    StreamDemux(s, s.payload.takeLow(log2Up(channels)).asUInt, channels).map(
      s => s.translateWith(s.payload.dropLow(log2Up(channels)).asUInt)
    )
  ).transpose
  val inArbiters = Array.fill(channels)(new StreamArbiter(UInt(addrWidth - log2Up(channels) bits), channels)(
    StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none
  ))
  (inArbiters, addrFromCmdDemux).zipped.foreach((tos, froms) => (tos.io.inputs, froms).zipped.foreach(_ << _))
  val addrFromArbiter = inArbiters.map(_.io.output)
  val addrSelect = inArbiters.map(_.io.chosen)
  val addrFromArbiterFork = addrFromArbiter.map(s => StreamFork(s, 2))
  val selectStream = (addrFromArbiterFork, addrSelect).zipped.map((s, sel) => Queue(s(1).translateWith(sel), 32))
  (io.addrToMem, addrFromArbiterFork).zipped.foreach((to, from) => to << from(0).toFlow)
  val dataFromMemQueue = io.dataFromMem.map(s => s.toStream.m2sPipe())
  val joinEvent = (dataFromMemQueue, selectStream).zipped.map((a, b) => StreamJoin(a, b).translateWith(a.payload))

  val dataSelect = selectStream.map(_.payload)
  val dataStreamDemux = (joinEvent, dataSelect).zipped.map((s, sel) =>
    StreamDemux(s, sel, channels)
  ).transpose

  val outArbiters = Array.fill(channels)(
    new StreamArbiter(dataType, channels)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.none)
  )
  (outArbiters, dataStreamDemux).zipped.foreach((tos, froms) => (tos.io.inputs, froms).zipped.foreach(_ << _))
  (io.dataToCmd, outArbiters).zipped.foreach((to, from) => to << from.io.output)
}
