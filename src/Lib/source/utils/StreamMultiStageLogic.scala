package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamMultiStageLogic {
  def apply[T1 <: Data, T2 <: Data](
                                     input: Stream[T1],
                                     output: Stream[T2],
                                     stage: Int,
                                     op: (T1, Vec[Bool]) => T2
                                   ): Unit = {
    require(stage >= 1)
    val m2sPipes = EventM2sPipes(stage)
    m2sPipes.io.eventIn.arbitrationFrom(input)
    output.arbitrationFrom(m2sPipes.io.eventOut)
    output.payload := op(input.payload, m2sPipes.io.CEs)
  }
}
