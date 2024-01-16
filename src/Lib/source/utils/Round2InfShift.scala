package utils

import spinal.core._

import scala.language.postfixOps

object Round2InfShift {
  def apply(in: SInt, shift: UInt): SInt = {
    val shiftResized = shift.resize(log2Up(in.getWidth))
    val bit = in(shiftResized - 1)
    val shifted = in >> shiftResized
    Mux(bit, shifted + 1, shifted)
  }
}

case class Round2InfShift(width: Int) extends Component {
  val io = new Bundle {
    val din = in SInt (width bits)
    val dout = out SInt (width bits)
    val shift = in UInt (log2Up(width) bits)
  }
  noIoPrefix()
  io.dout := Round2InfShift(io.din, io.shift).resized
}