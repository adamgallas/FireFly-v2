package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Linked[T <: Data, T2 <: Data](valueType: HardType[T], linkedType: HardType[T2]) extends Bundle {
  val value = valueType()
  val linked = linkedType()
}
