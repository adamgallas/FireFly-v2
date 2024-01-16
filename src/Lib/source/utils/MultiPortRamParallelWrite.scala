package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class MultiPortRamParallelWrite[T <: Data](dataType: HardType[T], channels: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val addrWidth = log2Up(bufferDepth)
  val readLatency = 4
  val io = new Bundle {
    val wrCmd = slave(Flow(Vec(utils.Linked(UInt(addrWidth bits), dataType), channels)))
    val rdCmds = slave(Flow(Vec(UInt(addrWidth bits), channels)))
    val rdData = master(Flow(Vec(dataType, channels)))
  }

  val ram = Array.fill(channels)(Mem(dataType, bufferDepth / channels))
  ram.foreach(_.addAttribute("ram_style", tech))

  (ram, io.wrCmd.payload).zipped.foreach((m, s) => m.write(
    address = s.value,
    data = s.linked,
    enable = io.wrCmd.valid
  ))

  val rdCtrl = MultiPortReadCtrl(dataType, channels, addrWidth, 2)

  val rdCmdsDly = Flow(Vec(UInt(addrWidth bits), channels))
  rdCmdsDly.valid := RegNext(io.rdCmds.valid, init = False)
  rdCmdsDly.payload := RegNext(io.rdCmds.payload)
  rdCtrl.io.inputCmds << rdCmdsDly

  val memCmdsDly = Flow(Vec(UInt(addrWidth - log2Up(channels) bits), channels))
  memCmdsDly.valid := RegNext(rdCtrl.io.memCmds.valid, init = False)
  memCmdsDly.payload := RegNext(rdCtrl.io.memCmds.payload)

  val memData = Flow(Vec(dataType, channels))
  memData.valid := RegNext(memCmdsDly.valid, init = False)
  (memData.payload, memCmdsDly.payload, ram).zipped.foreach((d, a, m) => d := m.readSync(a))
  rdCtrl.io.memData << memData

  val rdData = Flow(Vec(dataType, channels))
  rdData.valid := RegNext(rdCtrl.io.outputData.valid, init = False)
  rdData.payload := RegNext(rdCtrl.io.outputData.payload)
  io.rdData << rdData
}

object MultiPortRamParallelWrite {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new MultiPortRam(UInt(128 bits), 4, 8192))
  }
}