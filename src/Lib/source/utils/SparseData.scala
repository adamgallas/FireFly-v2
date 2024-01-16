package utils

import spinal.core._

import scala.language.postfixOps

case class SparseData[T <: Data](dataType: HardType[T], channels: Int) extends Bundle {
  val data = dataType()
  val index = UInt(log2Up(channels) bits)
  val indexOH = Bits(channels bits)
}
