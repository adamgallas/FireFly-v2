package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

class DSP48E2(attrs: DSP48E2Attributes) extends BlackBox {
  addGenerics(attrs.generics: _*)
  val CLK = in Bool()
  val INST = in(DSP48E2CONTROL())

  val CASCDATAIN = in(DSP48E2CASC())
  val CASCDATAOUT = out(DSP48E2CASC())

  val CEs = in(DSP48E2CEs())
  val RSTs = in(DSP48E2RSTs())

  val DATAIN = in(DSP48E2INPUT())
  val DATAOUT = out(DSP48E2OUTPUT())

  INST.setName("")
  DATAIN.setName("")
  DATAOUT.setName("")
  CASCDATAOUT.setAsCascadeOut()
  CASCDATAIN.setAsCascadeIn()

  CEs.all.foreach(_.default(False))
  RSTs.all.foreach(_.default(False))
  DATAIN.all.foreach(s => s.default(s.getZero))
  INST.all.foreach(s => s.default(s.getZero))

  CASCDATAIN.all.foreach(s => s.default(s.getZero))
  CASCDATAIN.MULTSIGN.default(CASCDATAIN.MULTSIGN.getZero)

  def preassign() = {
    import CEs._
    import attrs._
    val aPipe = AREG max ACASCREG
    val bPipe = BREG max BCASCREG
    aPipe match {
      case 0 => A1 := False; A2 := False
      case 1 =>
        if (attrs.USE_MULT == "MULTIPLY") {
          A1 := True
          A2 := False
        }
        else {
          A1 := False
          A2 := True
        }
      case 2 => A1 := True; A2 := True
    }
    bPipe match {
      case 0 => B1 := False; B2 := False
      case 1 =>
        if (attrs.USE_MULT == "MULTIPLY") {
          B1 := True
          B2 := False
        }
        else {
          B1 := False
          B2 := True
        }
      case 2 => B1 := True; B2 := True
    }
    if (CARRYINREG == 1 && CARRYINSELREG == 1) CTRL := True else CTRL := False
    val otherCEs = Seq(C, D, AD, M, P, CARRYIN, INMODE, ALUMODE)
    val otherREGs = Seq(CREG, DREG, ADREG, MREG, PREG, CARRYINREG, INMODEREG, ALUMODEREG)
    otherREGs.zip(otherCEs).foreach { case (i, bool) => if (i == 1) bool := True else bool := False }
    RSTs.all.foreach(_.clear())
  }

  mapClockDomain(clock = CLK)
}
