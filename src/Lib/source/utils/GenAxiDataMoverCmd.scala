package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object GenAxiDataMoverCmd {

  def apply(stream: Stream[utils.Linked[UInt, UInt]], addr: UInt, inc: Bool = True, eof: Bool = False) = {
    val cmd = Stream(Bits(72 bits))
    cmd.arbitrationFrom(stream)
    cmd.payload := B"00000000" ##
      (stream.value + addr).resize(32) ##
      B"0" ##
      eof ##
      B"000000" ##
      inc ##
      stream.linked.resize(23)
    cmd
  }
}
