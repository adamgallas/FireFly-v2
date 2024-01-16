package systolic.MxVxN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8ImgxWeightGenWrapper(
                                     dataWidth: (Int, Int),
                                     parallelism: (Int, Int, Int),
                                     conv2dBufferDepth: Int,
                                     weightBufferDepth: Int
                                   ) extends Component {
  require(dataWidth._1 % 8 == 0 && dataWidth._2 % 8 == 0)
  val (m, v, n) = parallelism
  val io = new Bundle {
    val inputs = slave(Stream(Bits(dataWidth._1 bits)))
    val weights = slave(Stream(Bits(dataWidth._2 bits)))
    val outputs = master(Flow(Fragment(utils.Linked(Vec(Vec(Bits(8 bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
  }

  val gen = new ImgWeightGen(Vec(Bits(8 bits), v), Vec(Vec(Bits(8 bits), v), m), n, n / 2, conv2dBufferDepth, weightBufferDepth)
  val imgGenCfg = gen.imgGenCfg.toIo()
  val weightGenCfg = gen.weightGenCfg.toIo()

  StreamWidthAdapter(io.inputs, gen.io.inputs)
  StreamWidthAdapter(io.weights, gen.io.weights)
  io.outputs << gen.io.outputs.toFlow
}
