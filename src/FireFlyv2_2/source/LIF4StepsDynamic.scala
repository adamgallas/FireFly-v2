import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class LIF4StepsDynamic(width: Int) extends NeuroDynamic(width) {

  val preProcess = new LIF4StepsDynamicPreProcess(width)
  val postProcess = new LIF4StepsDynamicPostProcess(width)

  preProcess.io.threshold := io.threshold
  preProcess.io.inputs := io.inputs.fragment
  postProcess.io.cmpRes := preProcess.io.cmpRes
  postProcess.io.addRes := preProcess.io.addRes
  postProcess.io.subRes := preProcess.io.subRes
  io.outSpikes.fragment := postProcess.io.outSpikes

  postProcess.io.valid := RegNext(io.inputs.valid, init = False)
  postProcess.io.last := RegNext(io.inputs.last, init = False)

  io.outSpikes.valid := RegNext(postProcess.io.valid, init = False)
  io.outSpikes.last := RegNext(postProcess.io.last, init = False)
}

object LIF4StepsDynamic extends App {
  SpinalVerilog(new LIF4StepsDynamic(16))
}