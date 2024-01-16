package utils

import spinal.core._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object ReduceTernaryTree {
  def apply[T <: Data](pimped: Seq[T], op: (T, T, T) => T, levelBridge: (T, Int) => T): T = {

    @tailrec
    def stage(elements: ArrayBuffer[T], level: Int): T = {
      if (elements.length == 1) return elements.head
      val stageLogic = new ArrayBuffer[T]()
      val logicCount = (elements.length + 2) / 3

      for (i <- 0 until logicCount) {
        if (i * 3 + 2 < elements.length)
          stageLogic += levelBridge(op(elements(i * 3), elements(i * 3 + 1), elements(i * 3 + 2)), level)
        else if (i * 3 + 1 < elements.length)
          stageLogic += levelBridge(op(elements(i * 3), elements(i * 3 + 1), elements.head.getZero), level)
        else
          stageLogic += levelBridge(elements(i * 3), level)
      }
      stage(stageLogic, level + 1)

    }

    val array = ArrayBuffer[T]() ++ pimped
    assert(array.nonEmpty)
    stage(array, 0)
  }
}