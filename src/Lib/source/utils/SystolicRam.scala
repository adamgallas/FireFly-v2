package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps


case class SystolicRamWrCmd[T <: Data](dataType: HardType[T], addrWidth: Int, selWidth: Int) extends Bundle {
  val address = UInt(addrWidth bits)
  val bankSel = UInt(selWidth bits)
  val data = dataType()
}

case class RamCell[T <: Data](dataType: HardType[T], depth: Int, selWidth: Int, bankID: Int, isBottom: Boolean = false) extends Component {
  val addrWidth = log2Up(depth)
  val io = new Bundle {
    val inWrCmd = slave(Flow(SystolicRamWrCmd(dataType, addrWidth, selWidth)))
    val inRdCmd = slave(Flow(UInt(addrWidth bits)))
    val rdData = master(Flow(dataType))
    val outWrCmd = if (!isBottom) master(Flow(SystolicRamWrCmd(dataType, addrWidth, selWidth))) else null
    val outRdCmd = if (!isBottom) master(Flow(UInt(addrWidth bits))) else null
  }
  val ram = Mem(dataType, depth)

  val hit = io.inWrCmd.bankSel === bankID && io.inWrCmd.valid
  ram.write(
    address = io.inWrCmd.address,
    data = io.inWrCmd.data,
    enable = hit
  )
  val readout = ram.readSync(io.inRdCmd.payload)
  io.rdData.valid := RegNext(io.inRdCmd.valid, init = False)
  io.rdData.payload := readout

  val pipe = !isBottom generate new Area {
    val wrCmd = RegNext(io.inWrCmd.payload)
    val rdCmd = RegNext(io.inRdCmd.payload)
    io.outWrCmd.valid := RegNext(io.inWrCmd.valid, init = False)
    io.outWrCmd.payload := wrCmd
    io.outRdCmd.valid := RegNext(io.inRdCmd.valid, init = False)
    io.outRdCmd.payload := rdCmd
  }
}

case class SystolicRam(width: Int, partition: Int, depth: Int) extends Component {

  require(width % partition == 0)

  val sliceWidth = width / partition
  val addrWidth = log2Up(depth)
  val bankSelWidth = log2Up(partition)

  val io = new Bundle {
    val wrCmd = slave(Flow(SystolicRamWrCmd(Bits(sliceWidth bits), addrWidth, bankSelWidth)))
    val rdCmd = slave(Flow(UInt(addrWidth bits)))
    val rdData = Vec(master(Flow(Bits(sliceWidth bits))), partition)
  }

  val banks = for (i <- 0 until partition) yield
    RamCell(Bits(sliceWidth bits), depth, bankSelWidth, i, i == partition - 1)

  io.wrCmd >> banks.head.io.inWrCmd
  io.rdCmd >> banks.head.io.inRdCmd

  for (i <- 0 until partition - 1) {
    banks(i).io.outWrCmd >> banks(i + 1).io.inWrCmd
    banks(i).io.outRdCmd >> banks(i + 1).io.inRdCmd
  }

  for (i <- 0 until partition) {
    banks(i).io.rdData >> io.rdData(i)
  }
}

object SystolicRam extends App {
  SpinalVerilog(new SystolicRam(512 * 8, 8, 512))
}