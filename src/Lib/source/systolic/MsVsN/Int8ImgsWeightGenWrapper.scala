package systolic.MsVsN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8ImgsWeightGenWrapper(
                                     dataWidth: (Int, Int),
                                     parallelism: (Int, Int, Int),
                                     conv2dBufferDepth: Int,
                                     weightBufferDepth: Int,
                                     t: Int = 1
                                   ) extends Component {
  require(dataWidth._1 % 8 == 0 && dataWidth._2 % 8 == 0)
  val (m, v, n) = parallelism
  val io = new Bundle {
    val inputs = slave(Stream(Bits(dataWidth._1 bits)))
    val weights = slave(Stream(Bits(dataWidth._2 bits)))
    val outputs = master(Flow(Fragment(utils.Linked(Vec(Vec(Bits(t bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
  }

  val gen = new ImgWeightGen(Vec(Bits(t bits), v), Vec(Vec(Bits(8 bits), v), m), n, n / 2, conv2dBufferDepth, weightBufferDepth)
  val imgGenCfg = gen.imgGenCfg.toIo()
  val weightGenCfg = gen.weightGenCfg.toIo()
  StreamWidthAdapter(io.inputs, gen.io.inputs)

  val factor = (8 * v * m) / dataWidth._2
  gen.io.weights << utils.StreamReshape(
    utils.StreamBigWidthAdapter(io.weights, factor),
    Bits(8 bits),
    (v, m)
  )
  io.outputs << gen.io.outputs.toFlow
}
