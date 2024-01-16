package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class APlusDMultB(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val A = in Bits (30 bits)
    val B = in Bits (18 bits)
    val D = in Bits (27 bits)
    val P = out Bits (48 bits)
    val CEs = in Vec(Bool(), 4)
  }
  noIoPrefix()

  if (useModel) {
    val Ad = RegNextWhen(io.A, io.CEs(0))
    val Bd = RegNextWhen(io.B, io.CEs(0))
    val Dd = RegNextWhen(io.D, io.CEs(0))
    val ADd = RegNextWhen(Ad.asSInt + Dd.asSInt, io.CEs(1))
    val Bdd = RegNextWhen(Bd.asSInt, io.CEs(1))
    val M = RegNextWhen(ADd * Bdd, io.CEs(2))
    io.P := RegNextWhen(M, io.CEs(3)).resize(48 bits).asBits
  }
  else {
    val attrs = new DSP48E2Attributes()
    attrs.BREG = 2
    attrs.AMULTSEL = "AD"

    val dsp = new DSP48E2(attrs)
    dsp.DATAIN.CARRYIN.clearAll()
    dsp.DATAIN.C.clearAll()
    dsp.DATAIN.A := io.A
    dsp.DATAIN.B := io.B
    dsp.DATAIN.D := io.D

    io.P := dsp.DATAOUT.P

    dsp.CASCDATAIN.all.foreach(_.clearAll())
    dsp.CASCDATAIN.MULTSIGN.clearAll()

    dsp.INST.CARRYINSEL.clearAll()
    dsp.INST.INMODE := B"0_0_1_0_1"
    dsp.INST.ALUMODE := B"0_0_0_0"
    dsp.INST.OPMODE := B"000_00_01_01"

    dsp.CEs.A1 := io.CEs(0)
    dsp.CEs.B1 := io.CEs(0)
    dsp.CEs.D := io.CEs(0)
    dsp.CEs.AD := io.CEs(1)
    dsp.CEs.B2 := io.CEs(1)
    dsp.CEs.M := io.CEs(2)
    dsp.CEs.P := io.CEs(3)

    dsp.CEs.CTRL.set()
    dsp.CEs.ALUMODE.set()
    dsp.CEs.INMODE.set()

    dsp.RSTs.all.foreach(_.clear())
  }
}
