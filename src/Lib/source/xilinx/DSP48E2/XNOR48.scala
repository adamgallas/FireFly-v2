package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class XNOR48(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val AB = in Bits (48 bits)
    val C = in Bits (48 bits)
    val P = out Bits (48 bits)
    val CEs = in Vec(Bool(), 2)
  }
  noIoPrefix()

  if (useModel) {
    val ABd = RegNextWhen(io.AB.as(Vec(Bool(), 48)), io.CEs(0))
    val Cd = RegNextWhen(io.C.as(Vec(Bool(), 48)), io.CEs(0))
    io.P := RegNextWhen(Vec((ABd, Cd).zipped.map((x, y) => ~(x ^ y))).asBits, io.CEs(1))
  }
  else {
    val attrs = new DSP48E2Attributes()
    attrs.MREG = 0
    attrs.USE_MULT = "NONE"
    attrs.USE_SIMD = "ONE48"

    val dsp = new DSP48E2(attrs)
    dsp.DATAIN.CARRYIN.clearAll()
    dsp.DATAIN.C := io.C
    dsp.DATAIN.D.clearAll()
    dsp.DATAIN.A := io.AB.takeHigh(30)
    dsp.DATAIN.B := io.AB.take(18)

    io.P := dsp.DATAOUT.P

    dsp.CASCDATAIN.all.foreach(_.clearAll())
    dsp.CASCDATAIN.MULTSIGN.clearAll()

    dsp.INST.CARRYINSEL.clearAll()
    dsp.INST.INMODE := B"1_0_0_0_1"
    dsp.INST.ALUMODE := B"0_1_0_1"
    dsp.INST.OPMODE := B"00_011_00_11"

    dsp.CEs.A2 := io.CEs(0)
    dsp.CEs.B2 := io.CEs(0)
    dsp.CEs.C := io.CEs(0)
    dsp.CEs.P := io.CEs(1)

    dsp.CEs.CTRL.set()
    dsp.CEs.ALUMODE.set()
    dsp.CEs.INMODE.set()

    dsp.RSTs.all.foreach(_.clear())
  }
}
