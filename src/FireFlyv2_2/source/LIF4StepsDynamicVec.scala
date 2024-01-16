import spinal.core._
import spinal.lib._

import scala.language.{dynamics, postfixOps}

class LIF4StepsDynamicVec(width: Int, length: Int) extends NeuroDynamicVec(width, length) {

  val dynamic = Array.fill(length)(new LIF4StepsDynamic(width))
//  dynamic.foreach(_.io.threshold := io.threshold)
  (dynamic, io.threshold).zipped.foreach(_.io.threshold := _)

  dynamic.foreach(_.io.inputs.valid := io.inputs.valid)
  dynamic.foreach(_.io.inputs.last := io.inputs.last)
  io.outSpikes.valid := dynamic.head.io.outSpikes.valid
  io.outSpikes.last := dynamic.last.io.outSpikes.last

  (dynamic, io.inputs.fragment).zipped.foreach(_.io.inputs.fragment := _)
  (io.outSpikes.fragment, dynamic).zipped.foreach(_ := _.io.outSpikes.fragment)
}

object LIF4StepsDynamicVec extends App {
  SpinalVerilog(new LIF4StepsDynamicVec(16, 16))
}