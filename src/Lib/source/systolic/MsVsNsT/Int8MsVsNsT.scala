package systolic.MsVsNsT

import spinal.core._
import spinal.lib._
import systolic.MsVsN._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8MsVsNsT(
                        ddrClk: ClockDomain,
                        dataWidth: (Int, Int),
                        parallelism: (Int, Int, Int, Int),
                        conv2dBufferDepth: Int,
                        weightBufferDepth: Int
                      ) extends Component {
  val (m, v, n, t) = parallelism
  val io = new Bundle {
    val inputs = slave(Stream(Bits(dataWidth._1 bits)))
    val weights = slave(Stream(Bits(dataWidth._2 bits)))
    val outputs = master(Flow(Fragment(Vec(Vec(Bits(24 bits), m), t))))
  }
  val matrix = Int8MsVsNsTMatrixWrapper(ddrClk, parallelism)
  val dataGen = Int8ImgsWeightGenWrapper(dataWidth, (m, v, n), conv2dBufferDepth, weightBufferDepth, t)
  val imgGenCfg = dataGen.imgGenCfg.toIo()
  val weightGenCfg = dataGen.weightGenCfg.toIo()

  io.inputs >> dataGen.io.inputs
  io.weights >> dataGen.io.weights
  dataGen.io.outputs >> matrix.io.inputs
  io.outputs << matrix.io.outputs
}
