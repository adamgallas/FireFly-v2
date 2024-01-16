package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MultiPortReadCtrl[T <: Data](dataType: HardType[T], channels: Int, addrWidth: Int, readLatency: Int = 2) extends Component {
  val io = new Bundle {
    val inputCmds = slave(Flow(Vec(UInt(addrWidth bits), channels)))
    val memCmds = master(Flow(Vec(UInt(addrWidth - log2Up(channels) bits), channels)))
    val memData = slave(Flow(Vec(dataType, channels)))
    val outputData = master(Flow(Vec(dataType, channels)))
  }

  val select = io.inputCmds.payload.map(_.takeLow(log2Up(channels)).asUInt)
  val addr = io.inputCmds.payload.map(_.dropLow(log2Up(channels)).asUInt)

  val selectOH = Vec(select.map(s => UIntToOh(s).as(Vec(Bool(), channels)))).transpose
  val dec = Vec(selectOH.map(s => OHToUInt(Vec(s).asBits)))
  val enc = Delay(Vec(select), readLatency)

  val routedAddr = Vec(dec.map(sel => addr(sel)))
  io.memCmds.payload := routedAddr
  io.memCmds.valid := io.inputCmds.valid

  val data = io.memData.payload
  val routedData = Vec(enc.map(sel => data(sel)))

  io.outputData.payload := routedData
  io.outputData.valid := io.memData.valid
}

object MultiPortReadCtrl {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new MultiPortReadCtrl(UInt(128 bits), 4, 12))
  }
}