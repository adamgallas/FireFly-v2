import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SewConnectAddOne2Two(length: Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Vec(Bits(4 bits), length)))
    val shortCut = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
  }

  val joinEvent = StreamJoin(io.input.toEvent(), io.shortCut.toEvent()).toEvent()
  val res = Vec((io.input.payload, io.shortCut.payload).zipped.map((ib, sb) => Vec((ib.asBools, sb.asBools).zipped.map(_.asUInt +^ _.asUInt))))
  val primitive = Stream(Vec(Bits(4 bits), length * 2))
  primitive.arbitrationFrom(joinEvent)
  for (l <- 0 until length) {
    primitive.payload(l) := res(l)(1) ## res(l)(0)
    primitive.payload(l + length) := res(l)(3) ## res(l)(2)
  }
  StreamWidthAdapter(primitive.m2sPipe(), io.output)
}

object SewConnectAddOne2Two extends App {
  SpinalVerilog(new SewConnectAddOne2Two(16))
}