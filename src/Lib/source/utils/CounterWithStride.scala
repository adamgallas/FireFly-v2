package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object CounterWithStride {
  def apply(width: Int, stride: Int) = new CounterWithStride(width, stride)
}

class CounterWithStride(val width: Int, val stride: Int) extends ImplicitArea[UInt] {
  val willIncrement = False.allowOverride
  val willClear = False.allowOverride

  def clear(): Unit = willClear := True

  def increment(): Unit = willIncrement := True

  val valueNext = UInt(width bit)
  val value = RegNext(valueNext) init 0

  valueNext := value
  when(willIncrement) {
    valueNext := value + stride
  }
  when(willClear) {
    valueNext.clearAll()
  }

  override def implicitValue: UInt = this.value
}