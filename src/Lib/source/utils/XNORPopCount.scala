package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class XNORPopCount(n: Int) extends Component {
  val io = new Bundle {
    val inputs = in Vec(Bool(), n)
    val weights = in Vec(Bool(), n)
    val cond = in Vec(Bool(), 3)
    val outputs = out UInt()
  }
  noIoPrefix()

  def xnor1(in: Array[(Bool, Bool)]) = {
    require(in.length == 1)
    val ret = UInt(2 bits)
    ret(0) := ~(in(0)._1 ^ in(0)._2)
    ret
  }

  def xnor2(in: Array[(Bool, Bool)]) = {
    require(in.length == 2)
    val ret = UInt(2 bits)
    val xnorRes = in.map(x => ~(x._1 ^ x._2))
    val (a, b) = (xnorRes(0), xnorRes(1))
    ret(0) := ((~a) & b) | (a & (~b))
    ret(1) := a & b
    ret
  }

  def xnor3(in: Array[(Bool, Bool)]) = {
    require(in.length == 3)
    val ret = UInt(2 bits)
    val xnorRes = in.map(x => ~(x._1 ^ x._2))
    val (a, b, c) = (xnorRes(0), xnorRes(1), xnorRes(2))
    ret(0) := ((~a) & (~b) & c) | ((~a) & b & (~c)) | (a & (~b) & (~c)) | (a & b & c)
    ret(1) := (a & b) | (b & c) | (a & c)
    ret
  }

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

  val pair = (io.inputs, io.weights).zipped.map((_, _)).toArray
  val pairGroup = pair.grouped(3)
  val xnorRes = RegNextWhen(Vec(pairGroup.map(
    x => x.length match {
      case 1 => xnor1(x)
      case 2 => xnor2(x)
      case 3 => xnor3(x)
    }
  ).toArray), io.cond(0))

  val xnorResGroup = xnorRes.toArray.grouped((xnorRes.length + 2) / 3).toArray
  val wallaceTreeRes = RegNextWhen(Vec(xnorResGroup.map(x => WallaceTree(Vec(x), fa, ha).implicitValue)), io.cond(0))
  require(wallaceTreeRes.length == 3)
  io.outputs := RegNextWhen(wallaceTreeRes(0) +^ wallaceTreeRes(1) +^ wallaceTreeRes(2), io.cond(1))
}
