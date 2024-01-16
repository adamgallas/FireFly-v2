package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class TWO24AddWithFastFetch(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val AB = in Bits (48 bits)
    val C = in Bits (48 bits)
    val P = out Bits (48 bits)
    val FastFetch = in Bool()
    val CEs = in Vec(Bool(), 2)
  }
  noIoPrefix()

  if (useModel) {
    val fastFetchd = RegNextWhen(io.FastFetch, io.CEs(0))
    val inputX = Vec(RegNextWhen(io.AB, io.CEs(0)).subdivideIn(24 bits).map(_.asSInt))
    val outputP = Vec(SInt(24 bits), 2)
    val inputZ = Mux(fastFetchd, outputP, Vec(io.C.subdivideIn(24 bits).map(_.asSInt)))

    outputP := Vec((inputX, inputZ).zipped.map((x, z) => RegNextWhen(x + z, io.CEs(1))))
    io.P.assignFromBits(outputP.asBits)
  }
  else {
    val attrs = new DSP48E2Attributes()
    attrs.MREG = 0
    attrs.CREG = 0
    attrs.USE_MULT = "NONE"
    attrs.USE_SIMD = "TWO24"

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
    dsp.INST.ALUMODE := B"0_0_0_0"
    dsp.INST.OPMODE := B"00_0" ## io.FastFetch.asBits ## B"0_1111"

    dsp.CEs.A2 := io.CEs(0)
    dsp.CEs.B2 := io.CEs(0)
    dsp.CEs.CTRL := io.CEs(0)
    dsp.CEs.P := io.CEs(1)

    dsp.CEs.ALUMODE.set()
    dsp.CEs.INMODE.set()

    dsp.RSTs.all.foreach(_.clear())
  }
}
