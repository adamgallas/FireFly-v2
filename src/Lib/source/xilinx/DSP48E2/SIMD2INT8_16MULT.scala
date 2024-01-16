package xilinx.DSP48E2

import spinal.core._

import scala.language.postfixOps

case class SIMD2INT8_16MULT(useModel: Boolean = false) extends Component {
  val io = new Bundle {
    val a = in SInt (8 bits)
    val b = in SInt (8 bits)
    val c = in SInt (8 bits)
    val ab = out SInt (16 bits)
    val ac = out SInt (16 bits)
    val CEs = in Vec(Bool(), 4)
  }
  noIoPrefix()

  val mult = APlusDMultB(useModel)
  mult.io.CEs := io.CEs
  mult.io.A := io.b.resize(30 bits).asBits
  //mult.io.D := io.c ## B(27 - 8 bits, default -> false)
  mult.io.D := io.c.expand ## B(27 - 9 bits, default -> false)
  mult.io.B := io.a.resize(18 bits).asBits

  val ab = mult.io.P.take(16).asSInt
  //val ac = mult.io.P.drop(19).take(16).asSInt
  val ac = mult.io.P.drop(18).take(16).asSInt
  io.ab := ab
  io.ac := Mux(ab(15), ac + 1, ac)
}
