package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class DSP48E2ALU(SIMD: Int, useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val AB = in Bits (48 bits)
    val C = in Bits (48 bits)
    val P = out Bits (48 bits)

    val PCascIn = in Bits (48 bits) default B(0, 48 bits)
    val PCascOut = out Bits (48 bits)

    val W = in Bits (2 bits)
    val X = in Bits (2 bits)
    val Y = in Bits (2 bits)
    val Z = in Bits (3 bits)
    val CEs = in Vec(Bool(), 2)
  }
  noIoPrefix()
  if (useModel) {
    val ABd = RegNextWhen(io.AB, io.CEs(0))
    val Cd = RegNextWhen(io.C, io.CEs(0))
    val Wd = RegNextWhen(io.W, io.CEs(0))
    val Xd = RegNextWhen(io.X, io.CEs(0))
    val Yd = RegNextWhen(io.Y, io.CEs(0))
    val Zd = RegNextWhen(io.Z, io.CEs(0))

    val P = Bits(48 bits)
    val Pd = RegNextWhen(P, io.CEs(1))

    val wVec = Vec(B(0, 48 bits), Pd, B(0, 48 bits), Cd)
    val xVec = Vec(B(0, 48 bits), B(0, 48 bits), Pd, ABd)
    val yVec = Vec(B(0, 48 bits), B(0, 48 bits), B(0, 48 bits), Cd)
    val zVec = Vec(B(0, 48 bits), io.PCascIn, Pd, Cd, B(0, 48 bits), B(0, 48 bits), B(0, 48 bits), B(0, 48 bits))

    val w = wVec(Wd.asUInt).subdivideIn(SIMD slices)
    val x = xVec(Xd.asUInt).subdivideIn(SIMD slices)
    val y = yVec(Yd.asUInt).subdivideIn(SIMD slices)
    val z = zVec(Zd.asUInt).subdivideIn(SIMD slices)

    val res = for (i <- 0 until SIMD) yield w(i).asSInt + x(i).asSInt + y(i).asSInt + z(i).asSInt
    P.assignFromBits(Vec(res).asBits)
    io.PCascOut := Pd
    io.P := Pd
  }
  else {
    val attrs = new DSP48E2Attributes()
    attrs.MREG = 0
    attrs.USE_MULT = "NONE"
    attrs.USE_SIMD = "FOUR12"

    val dsp = new DSP48E2(attrs)
    dsp.DATAIN.CARRYIN.clearAll()
    dsp.DATAIN.A := io.AB.takeHigh(30)
    dsp.DATAIN.B := io.AB.take(18)
    dsp.DATAIN.C := io.C
    dsp.DATAIN.D.clearAll()

    io.P := dsp.DATAOUT.P
    io.PCascOut := dsp.CASCDATAOUT.P

    dsp.CASCDATAIN.A.clearAll()
    dsp.CASCDATAIN.B.clearAll()
    dsp.CASCDATAIN.P := io.PCascIn
    dsp.CASCDATAIN.CARRYCAS.clearAll()
    dsp.CASCDATAIN.MULTSIGN.clearAll()

    dsp.INST.CARRYINSEL.clearAll()
    dsp.INST.INMODE := B"1_0_0_0_1"
    dsp.INST.ALUMODE := B"0_0_0_0"
    dsp.INST.OPMODE := io.W ## io.Z ## io.Y ## io.X

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
