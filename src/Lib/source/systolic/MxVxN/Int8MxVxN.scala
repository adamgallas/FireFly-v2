package systolic.MxVxN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8MxVxN(
                      ddrClk: ClockDomain,
                      dataWidth: (Int, Int),
                      parallelism: (Int, Int, Int),
                      conv2dBufferDepth: Int,
                      weightBufferDepth: Int
                    ) extends Component {
  val (m, v, n) = parallelism
  val io = new Bundle {
    val inputs = slave(Stream(Bits(dataWidth._1 bits)))
    val weights = slave(Stream(Bits(dataWidth._2 bits)))
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), m))))
  }
  val matrix = Int8MxVxNMatrixWrapper(ddrClk, parallelism)
  val dataGen = Int8ImgxWeightGenWrapper(dataWidth, parallelism, conv2dBufferDepth, weightBufferDepth)
  val imgGenCfg = dataGen.imgGenCfg.toIo()
  val weightGenCfg = dataGen.weightGenCfg.toIo()

  io.inputs >> dataGen.io.inputs
  io.weights >> dataGen.io.weights
  dataGen.io.outputs >> matrix.io.inputs
  io.outputs << matrix.io.outputs
}
