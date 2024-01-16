package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class FOUR12MuxAdd(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val AB = in Bits (48 bits)
    val C = in Bits (48 bits)
    val P = out Bits (48 bits)
    val ABSel = in Bool()
    val CSel = in Bool()
    val CEs = in Vec(Bool(), 2)
  }
  noIoPrefix()

  if (useModel) {
    val ABSubDiv = RegNextWhen(io.AB, io.CEs(0)).subdivideIn(12 bits)
    val CSubDiv = RegNextWhen(io.C, io.CEs(0)).subdivideIn(12 bits)
    val ABSel = RegNextWhen(io.ABSel, io.CEs(0))
    val CSel = RegNextWhen(io.CSel, io.CEs(0))
    val res = Vec((ABSubDiv, CSubDiv).zipped.map((x, y) => RegNextWhen(
      Mux(ABSel, x.asSInt, S(0)) + Mux(CSel, y.asSInt, S(0)), io.CEs(1)
    )))
    io.P.assignFromBits(res.asBits)
  }
  else {
    val attrs = new DSP48E2Attributes()
    attrs.MREG = 0
    attrs.USE_MULT = "NONE"
    attrs.USE_SIMD = "FOUR12"

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
    dsp.INST.OPMODE := B"00000" ## io.CSel ## io.CSel ## io.ABSel ## io.ABSel

    dsp.CEs.A2 := io.CEs(0)
    dsp.CEs.B2 := io.CEs(0)
    dsp.CEs.C := io.CEs(0)
    dsp.CEs.CTRL := io.CEs(0)
    dsp.CEs.P := io.CEs(1)

    dsp.CEs.ALUMODE.set()
    dsp.CEs.INMODE.set()

    dsp.RSTs.all.foreach(_.clear())
  }
}
