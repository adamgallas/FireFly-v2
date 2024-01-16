package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ReverseMapping(channels: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Stream(UInt(log2Up(channels) bits))), channels)
    val outputs = Vec(master(Stream(UInt(log2Up(channels) bits))), channels)
  }
  noIoPrefix()

  for (i <- 0 until channels) {
    io.inputs(i).ready.clear()
    io.outputs(i).payload.clearAll()
    io.outputs(i).valid.clear()
  }

  for (i <- 0 until channels) {
    for (j <- 0 until channels) {
      when(io.inputs(j).payload === i) {
        io.outputs(i) << io.inputs(j)
      }
    }
  }
}

object ReverseMapping {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new ReverseMapping(4))
  }
}