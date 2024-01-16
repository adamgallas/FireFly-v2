package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DataDoubleClockRate[T <: Data](dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(Vec(dataType, 2))))
    val outputs = master(Flow(Fragment(dataType)))
  }
  noIoPrefix()

  val inValid = RegNext(io.inputs.valid, init = False)
  val inLast = RegNext(io.inputs.last, init = False)
  val inPayload = RegNext(io.inputs.fragment)

  val sel = Bool() setAsReg() init false
  sel.toggleWhen(inValid)

  val data = Mux(sel, inPayload(1), inPayload(0))
  val last = sel && inLast

  io.outputs.fragment := RegNext(data)
  io.outputs.last := RegNext(last, init = False)
  io.outputs.valid := RegNext(inValid, init = False)

//  val sel = Bool() setAsReg() init false
//  sel.toggleWhen(io.inputs.valid)
//
//  val data = Mux(sel, io.inputs.fragment(1), io.inputs.fragment(0))
//  val last = sel && io.inputs.last
//  val valid = io.inputs.valid
//
//  io.outputs.fragment := RegNext(data)
//  io.outputs.last := RegNext(last, init = False)
//  io.outputs.valid := RegNext(valid, init = False)
}
