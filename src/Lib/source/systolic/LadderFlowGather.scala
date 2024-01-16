package systolic

import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

class LadderFlowGather[T <: Data](dataType: HardType[T], length: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Flow(dataType())), length)
    val outputs = master(Flow(Fragment(dataType())))
  }
  val valid = RegNext(io.inputs.map(_.valid).reduce(_ || _), init = False)
  val payload = RegNext(Vec(io.inputs.map(_.payload)))
  val select = UInt(log2Up(length) bits) setAsReg() init 0
  select.addAttribute("MAX_FANOUT", "50")
  when(valid) {
    select := select + 1
  }
  val muxOut = payload(select)

  io.outputs.valid := RegNext(valid, init = False)
  io.outputs.fragment := RegNext(muxOut)
  io.outputs.last := Delay(io.inputs.last.valid, 2, init = False)
}
