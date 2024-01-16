package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class WallaceAdder(n: Int, width: Int) extends Component {

  def fa(dataIn: Seq[Bool]) = {
    require(dataIn.size == 3)
    val sum = dataIn.xorR
    val carry = MajorityVote(dataIn.asBits())
    (sum, carry)
  }

  def ha(dataIn: Seq[Bool]) = {
    require(dataIn.size == 2)
    val sum = dataIn.xorR
    val carry = dataIn.andR
    (sum, carry)
  }

  val dataIn = in Vec(UInt(width bits), n)
  val dataOut = out UInt()
  dataOut := WallaceTree(dataIn, fa, ha)
}
