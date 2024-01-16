import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SewConnectIAND(length: Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Vec(Bits(4 bits), length)))
    val shortCut = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
  }
  val joinEvent = StreamJoin(io.input.toEvent(), io.shortCut.toEvent()).toEvent()
  val res = (io.input.payload, io.shortCut.payload).zipped.map((ib, sb) => Vec((ib.asBools, sb.asBools).zipped.map(~_ & _)).asBits)
  io.output.arbitrationFrom(joinEvent)
  io.output.payload := Vec(res)
}

object SewConnectIAND extends App{
  SpinalVerilog(new SewConnectIAND(16))
}