import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SewConnectAddTwo2Two(length: Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Vec(Bits(4 bits), length)))
    val shortCut = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
  }

  val shortCut = Stream(Vec(Vec(Bits(4 bits), length), 2))
  StreamWidthAdapter(io.shortCut.m2sPipe(), shortCut)
  val joinEvent = StreamJoin(io.input.toEvent(), shortCut.toEvent()).toEvent()
  val resStream2Bits = Stream(Vec(Bits(4 bits), length * 2))

  for (l <- 0 until length) {
    val a = shortCut.payload(0)(l).takeLow(2).asUInt.expand + io.input.payload(l)(0).asUInt
    val b = shortCut.payload(0)(l).takeHigh(2).asUInt.expand + io.input.payload(l)(1).asUInt
    val c = shortCut.payload(1)(l).takeLow(2).asUInt.expand + io.input.payload(l)(2).asUInt
    val d = shortCut.payload(1)(l).takeHigh(2).asUInt.expand + io.input.payload(l)(3).asUInt

    resStream2Bits.payload(l) := b.sat(1) ## a.sat(1)
    resStream2Bits.payload(l + length) := d.sat(1) ## c.sat(1)
  }

  resStream2Bits.arbitrationFrom(joinEvent)
  val output2Bits = Stream(Vec(Bits(4 bits), length))
  StreamWidthAdapter(resStream2Bits.m2sPipe(), output2Bits)
  io.output << output2Bits
}

object SewConnectAddTwo2Two extends App {
  SpinalVerilog(new SewConnectAddTwo2Two(16))
}
