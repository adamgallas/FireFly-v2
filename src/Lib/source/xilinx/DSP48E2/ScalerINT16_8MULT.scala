package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class ScalerINT16_8MULT(
                        preShiftBits: Int,
                        dataWidth: Int,
                        useModel: Boolean = false
                      ) extends Component {
  require(dataWidth < 27)
  val io = new Bundle {
    val scalerBase = in UInt (16 bits)
    val scalerExpr = in UInt (4 bits)
    val CEs = in Vec(Bool(), 6)

    val din = in SInt (dataWidth bits)
    val dout = out SInt (8 bits)
  }
  noIoPrefix()
  val mult = AMultB(useModel)
  mult.io.A := io.din.resize(30 bits).asBits
  mult.io.B := io.scalerBase.expand.asSInt.resize(18 bits).asBits
  mult.io.CEs := Vec(io.CEs.take(4))

  val multRes = mult.io.P.asSInt
  val shiftLv1 = multRes >> preShiftBits
  val shiftLv2 = RegNextWhen(utils.Round2InfShift(shiftLv1, io.scalerExpr), io.CEs(4))
  io.dout := RegNextWhen(shiftLv2.fixTo(7 downto 0), io.CEs(5))
}
