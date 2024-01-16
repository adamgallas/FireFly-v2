package systolic

import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

class Accumulator() extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(Vec(Bits(24 bits), 2))))
    val outputs = master(Flow(Vec(Bits(24 bits), 2)))
  }

  val pValid = Bool() setAsReg() init False
  pValid.setWhen(io.inputs.valid).clearWhen(io.inputs.last)

  val lastDly = Delay(io.inputs.last, 2, init = False)
  val dsp48 = xilinx.DSP48E2.DSP48E2Build().genDynamicALUDSP48E2(
    pipe = (1, 1, 1, 0, 0),
    enable = (io.inputs.valid, pValid, False, False),
    mode = "2"
  )
  dsp48.DATAIN.C.assignFromBits(io.inputs.fragment.asBits)
  io.outputs.payload.assignFromBits(dsp48.DATAOUT.P)
  io.outputs.valid := lastDly
}
