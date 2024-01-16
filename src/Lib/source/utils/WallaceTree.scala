package utils

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** Summing n unsigned integers by Wallace/Dadda Tree
 */
case class WallaceTree(dataIn: Vec[UInt], compressor32: Seq[Bool] => (Bool, Bool), compressor22: Seq[Bool] => (Bool, Bool)) extends ImplicitArea[UInt] {

  val k = dataIn.map(_.getBitsWidth).max
  val n = dataIn.size
  val maxWidth = k + log2Up(n) - 1

  // build the original table
  val original = ArrayBuffer.fill(maxWidth)(ArrayBuffer[Bool]())
  //println(original.map(_.size).mkString(" "))
  dataIn.foreach(uint => uint.asBools.zip(original).foreach { case (bool, buffer) => buffer += bool })
  //println(original.map(_.size).mkString(" "))

  def compress(bits: ArrayBuffer[ArrayBuffer[Bool]]): ArrayBuffer[ArrayBuffer[Bool]] = {
    val rows = bits.map(_.size).max
    // println(s"rows remained: $rows")
    // println(bits.map(_.size).mkString(" "))
    if (rows == 2) bits
    else {
      val next = ArrayBuffer.fill(maxWidth)(ArrayBuffer[Bool]())
      bits.zipWithIndex.filter(_._1.nonEmpty).foreach { case (column, i) => // iterate column-by-column
        val groupedBits = column.grouped(3).toSeq
        val tobeCompressed = if (groupedBits.last.size == 3) groupedBits else groupedBits.init
        val remained = if (groupedBits.last.size == 3) ArrayBuffer[Bool]() else groupedBits.last
        // processing the grouped part
        val compressed = tobeCompressed.map(compressor32)
        compressed.foreach { case (sum, carry) =>
          next(i) += sum
          next(i + 1) += carry
        }
        // processing the remained part
        if (remained.size == 2 && rows == 3 && next(i).nonEmpty) {
          val (sum, carry) = compressor22(remained)
          next(i) += sum
          next(i + 1) += carry
        } else next(i) ++= remained
      }
      compress(next)
    }
  }

  val bitsForAddition = compress(original).map(_.padTo(2, False))

  override def implicitValue: UInt = bitsForAddition.map(_ (0)).asBits().asUInt +^ bitsForAddition.map(_ (1)).asBits().asUInt

  /** Calculate the maximum number n(h) of inputs for an h-level CSA tree
   *
   * n(h) = floor(3n(h − 1)/2)
   */
  def nh(h: Int): Int = h match {
    case 0 => 2
    case _ => 3 * nh(h - 1) / 2
  }
}
