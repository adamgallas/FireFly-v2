package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class DSP48E2INPUT() extends Bundle {
  val A = in Bits (30 bits)
  val B = in Bits (18 bits)
  val C = in Bits (48 bits)
  val D = in Bits (27 bits)
  val CARRYIN = in Bits (1 bits)
  val all = Seq(A, B, C, D, CARRYIN)
}

case class DSP48E2OUTPUT() extends Bundle {
  val P = out Bits (48 bits)
  val CARRYOUT = out Bits (4 bits)
  val XOROUT = out Bits (8 bits)
  val OVERFLOW, UNDERFLOW = out Bool()
  val PATTERNBDETECT, PATTERNDETECT = out Bool()
}

case class DSP48E2CONTROL() extends Bundle {
  val ALUMODE = Bits(4 bits)
  val INMODE = Bits(5 bits)
  val OPMODE = Bits(9 bits)
  val CARRYINSEL = Bits(3 bits)
  val all = Seq(ALUMODE, INMODE, OPMODE, CARRYINSEL)
}

case class DSP48E2CASC() extends Bundle {
  val A = Bits(30 bits)
  val B = Bits(18 bits)
  val P = Bits(48 bits)
  val CARRYCAS = Bits(1 bits)
  val MULTSIGN = Bits(1 bits)
  val all = Seq(A, B, P, CARRYCAS)

  def setAsCascadeOut(): Unit = {
    all.foreach(signal => signal.setName(signal.getPartialName() + "COUT"))
    this.MULTSIGN.setName("MULTSIGNOUT")
  }

  def setAsCascadeIn(): Unit = {
    all.foreach(signal => signal.setName(signal.getPartialName() + "CIN"))
    this.MULTSIGN.setName("MULTSIGNIN")
  }
}

case class DSP48E2CEs() extends Bundle {
  val A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all = Seq(A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("CE" + signal.getPartialName()))
}

case class DSP48E2RSTs() extends Bundle {
  val A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all = Seq(A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("RST" + signal.getPartialName()))
}