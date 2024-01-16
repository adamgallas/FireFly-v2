import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SewConnect(length: Int) extends Component {
  val io = new Bundle {
    val muxSel = in UInt (2 bits)

    val input = slave(Stream(Vec(Bits(4 bits), length)))
    val shortCut = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
  }

  val inputs = StreamDemux(io.input, io.muxSel, 3)
  val shortCuts = StreamDemux(io.shortCut, io.muxSel, 3)
  val outputs = Vec(Stream(Vec(Bits(4 bits), length)), 3)
  io.output << StreamMux(io.muxSel, outputs)

  val IAND = new SewConnectIAND(length)
  val One2Two = new SewConnectAddOne2Two(length)
  val Two2Two = new SewConnectAddTwo2Two(length)

  IAND.io.input << inputs(0)
  One2Two.io.input << inputs(1)
  Two2Two.io.input << inputs(2)

  IAND.io.shortCut << shortCuts(0)
  One2Two.io.shortCut << shortCuts(1)
  Two2Two.io.shortCut << shortCuts(2)

  outputs(0) << IAND.io.output
  outputs(1) << One2Two.io.output
  outputs(2) << Two2Two.io.output
}

object SewConnect extends App{
  SpinalVerilog(new SewConnect(16))
}