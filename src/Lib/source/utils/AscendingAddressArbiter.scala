package utils

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class AscendingAddressArbiter[T <: Data](dataType: HardType[T], addrWidth: Int, ports: Int) extends Component {
  val io = new Bundle {
    val toMemCmd = master Flow UInt(addrWidth bits)
    val fromMemData = slave Flow dataType()

    val cmdPorts = Vec(slave Stream Fragment(UInt(addrWidth bits)), ports)
    val dataPorts = Vec(master Flow dataType(), ports)
  }

  val cnt = UInt(log2Up(ports) bits) setAsReg() init 0
  val cntNext = cnt + io.cmdPorts.map(s => (s.fire & s.last).asUInt).reduce(_ + _)
  cnt := cntNext
  val overflow = cntNext.takeHigh(1).asBool
  when(overflow)(cnt.clearAll())

  val cond = Vec(Bool() setAsReg() init false, ports)
  (cond, io.cmdPorts).zipped.foreach((c, s) => c.setWhen(s.fire & s.last).clearWhen(overflow))
  val cmdPorts = io.cmdPorts.zip(cond).map(z => z._1.haltWhen(z._2))

  val value = cmdPorts.map(s => Mux(s.valid, s.fragment, s.fragment.getAllTrue))
  val minValue = value.reduce(_ min _)
  val matchOneHot = Vec(value.map(_ === minValue))
  (cmdPorts, matchOneHot).zipped.foreach(_.ready := _)

  val toMemCmdValid = cmdPorts.map(_.valid).orR
  val toMemCmdPayload = minValue

  io.toMemCmd.valid := Delay(toMemCmdValid, 2, init = False)
  io.toMemCmd.payload := Delay(toMemCmdPayload, 2)

  val oneHotFifo = StreamFifoHighPerf(matchOneHot, 32)
  oneHotFifo.io.push.valid := toMemCmdValid
  oneHotFifo.io.push.payload := matchOneHot
  oneHotFifo.io.pop.ready := io.fromMemData.valid

  (io.dataPorts, oneHotFifo.io.pop.payload).zipped.foreach(_.valid := _ & io.fromMemData.valid)
  io.dataPorts.foreach(_.payload := io.fromMemData.payload)
}
