package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamReduceTernaryTree {
  def apply[T <: Data](input: Stream[Vec[T]], output: Stream[T], op: (T, T, T) => T): Unit = {

    def reduceOp(vec: Vec[T], cond: Vec[Bool]): T = {
      ReduceTernaryTree(vec, op, (s: T, l: Int) => {
        RegNextWhen(s, cond(l))
      })
    }

    StreamMultiStageLogic(input, output, log3Up(input.payload.length), reduceOp)
  }
}
