package systolic

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class SlowDownBuffer[T <: Data](ddrClk: ClockDomain, dataType: HardType[T], length: Int, depth: Int) extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(dataType)))
    val outputs = master(Flow(Fragment(dataType)))
  }

  val buffer = Mem(dataType, depth)
  buffer.addAttribute("ram_style", "distributed")

  val ddr = new ClockingArea(ddrClk) {
    val waddr = Reg(UInt(log2Up(depth) bits)) init (0)
    waddr.addAttribute("MAX_FANOUT", "50")

    when(io.inputs.valid) {
      waddr := waddr + 1
      when(io.inputs.last) {
        waddr.clearAll()
      }
    }
    buffer.write(waddr, io.inputs.fragment, io.inputs.valid)
  }

  val set = Bool() setAsReg() init false addTag (crossClockDomain)
  val raddr = Reg(UInt(log2Up(depth) bits)) init (0)
  raddr.addAttribute("MAX_FANOUT", "50")

  val last = raddr === length - 1
  set.clearWhen(last).setWhen(io.inputs.valid)
  when(set) {
    raddr := raddr + 1
    when(last) {
      raddr.clearAll()
    }
  }
  val valid = RegNext(set, init = False)
  io.outputs.valid := valid
  io.outputs.last := RegNext(last, init = False)
  io.outputs.fragment := buffer.readSync(raddr)
}
