package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Bits2Addr(width: Int, length: Int) extends Component {
  require(isPow2(width))
  val cntWidth = log2Up(length)
  val io = new Bundle {
    val inputs = slave(Stream(Fragment(Bits(width bits))))
    val addr = master(Stream(UInt(cntWidth + log2Up(width) bits)))
  }

  val inputs = io.inputs.s2mPipe().m2sPipe()

  val cnt = UInt(cntWidth bits) setAsReg() init 0
  when(inputs.fire) {
    cnt := cnt + 1
    when(inputs.last) {
      cnt.clearAll()
    }
  }

  val filtered = inputs.toEvent().throwWhen(inputs.fragment === 0)
  val detector = Bits2Index(width)
  val addr = Stream(UInt(cntWidth + log2Up(width) bits))

  detector.io.inputs.arbitrationFrom(filtered)
  detector.io.inputs.payload := inputs.payload
  addr.arbitrationFrom(detector.io.outputs)
  addr.payload.assignFromBits(cnt ## detector.io.outputs.payload)

  io.addr << addr.s2mPipe().m2sPipe()
}
