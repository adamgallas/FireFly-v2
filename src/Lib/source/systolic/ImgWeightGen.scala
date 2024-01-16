package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ImgWeightGen[TI <: Data, TW <: Data](
                                            inDataType: HardType[TI],
                                            weightDataType: HardType[TW],
                                            pixelParallelism: Int,
                                            haltCycles: Int,
                                            conv2dBufferDepth: Int,
                                            weightBufferDepth: Int
                                          ) extends Component {
  val io = new Bundle {
    val inputs = slave(Stream(inDataType))
    val weights = slave(Stream(weightDataType))
    val outputs = master(Stream(Fragment(utils.Linked(Vec(inDataType, pixelParallelism), weightDataType))))
  }
  noIoPrefix()

  val imgGen = new conv2d.Conv2d(inDataType, pixelParallelism, conv2dBufferDepth)
  val weightGen = new fifos.StreamCycleFifo(weightDataType, weightBufferDepth)

  val imgGenCfg = imgGen.config.toIo()
  val weightGenCfg = new Bundle {
    val reuse = in UInt (8 bits)
    val length = in UInt (log2Up(weightBufferDepth) bits)
  }
  weightGen.io.reuse := weightGenCfg.reuse
  weightGen.io.length := weightGenCfg.length
  io.inputs >> imgGen.io.push
  io.weights >> weightGen.io.push

  val join = StreamJoin(imgGen.io.pop.m2sPipe(), weightGen.io.pop.m2sPipe())
  val packets = utils.StreamAddLast(join, weightGenCfg.length)
  val halted = utils.StreamHaltWhenLast(packets, haltCycles)
  io.outputs.arbitrationFrom(halted)
  io.outputs.last := halted.last
  io.outputs.value := halted._1
  io.outputs.linked := halted._2
}
