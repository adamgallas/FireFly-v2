package systolic

import spinal.lib._
import spinal.core._
import fifos._

import scala.language.postfixOps

case class MemReadPortSimpleArbiter[T <: Data](dataType: HardType[T], channels: Int, addrWidth: Int, readLatency: Int = 1) extends Component {
  val io = new Bundle {
    val addrFromCmd = slave(Flow(Vec(UInt(addrWidth bits), channels)))
    val addrToMem = master(Flow(Vec(UInt(addrWidth - log2Up(channels) bits), channels)))
    val dataFromMem = slave(Flow(Vec(dataType, channels)))
    val dataToCmd = master(Flow(Vec(dataType, channels)))
  }
  noIoPrefix()

  val select = io.addrFromCmd.payload.map(_.takeLow(log2Up(channels)).asUInt)
  val addr = io.addrFromCmd.payload.map(_.dropLow(log2Up(channels)).asUInt)
  val routedAddr = Vec(UInt(addrWidth - log2Up(channels) bits), channels)
  val selectTable = Vec(Vec(Bool, channels), channels)
  val selectTableDly = Vec(Vec(Bool, channels), channels)

  for (i <- 0 until channels) {
    for (j <- 0 until channels) {
      selectTable(i)(j) := select(j) === i
      selectTableDly(i)(j) := Delay(selectTable(i)(j), readLatency, init = False)
    }
  }

  for (i <- 0 until channels) {
    routedAddr(i).clearAll()
    for (j <- 0 until channels) {
      when(selectTable(i)(j)) {
        routedAddr(i) := addr(j)
      }
    }
  }
  io.addrToMem.payload := routedAddr
  io.addrToMem.valid := io.addrFromCmd.valid

  val data = io.dataFromMem.payload
  val routedData = Vec(dataType, channels)
  for (i <- 0 until channels) {
    routedData(i) := dataType().getZero
    for (j <- 0 until channels) {
      when(selectTableDly(i)(j)) {
        routedData(i) := data(j)
      }
    }
  }
  io.dataToCmd.payload := routedData
  io.dataToCmd.valid := io.dataFromMem.valid
}

object MemReadPortSimpleArbiter {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new MemReadPortSimpleArbiter(UInt(32 bits), 8, 10))
  }
}