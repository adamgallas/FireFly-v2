package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamReduceBalancedTree {
  def apply[T <: Data](input: Stream[Vec[T]], output: Stream[T], op: (T, T) => T): Unit = {

    def reduceOp(vec: Vec[T], cond: Vec[Bool]): T = {
      vec.reduceBalancedTree(op, (s, l) => {
        RegNextWhen(s, cond(l))
      })
    }

    StreamMultiStageLogic(input, output, log2Up(input.payload.length), reduceOp)
  }
}
