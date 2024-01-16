package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Bits2Index(width: Int) extends Component {
  val io = new Bundle {
    val inputs = slave(Stream(Bits(width bits)))
    val outputs = master(Stream(UInt(log2Up(width) bits)))
  }

  val storeMask = Bits(width bits) setAsReg() init 0
  val payload = io.inputs.payload
  val masked = payload & ~storeMask
  val lsb = OHMasking.firstV2(masked)
  val index = OHToUInt(lsb)
  val newMask = lsb | storeMask
  val updateMasked = masked & ~lsb

  when(io.outputs.fire)(storeMask := newMask)
  when(io.inputs.fire)(storeMask.clearAll())

  val last = !updateMasked.orR
  io.inputs.ready := io.outputs.ready && last
  io.outputs.valid := io.inputs.valid
  io.outputs.payload := index
}