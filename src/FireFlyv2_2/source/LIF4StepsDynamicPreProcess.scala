import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class LIF4StepsDynamicPreProcess(width: Int) extends Component {
  val io = new Bundle {
    val threshold = in SInt (width bits)
    val inputs = in Vec(SInt(width bits), 4)

    val cmpRes = out Vec(Bool(), 10)
    val subRes = out Vec(SInt(width + 2 bits), 4)
    val addRes = out Vec(SInt(width + 2 bits), 4)
  }

  val A = io.inputs(0) >> 1
  val B = io.inputs(1) >> 1
  val C = io.inputs(2) >> 1
  val D = io.inputs(3) >> 1
  val AB = (A >> 1) + B.expand
  val BC = (B >> 1) + C.expand
  val CD = (C >> 1) + D.expand
  val ABC = (AB >> 1) + C.expand
  val BCD = (BC >> 1) + D.expand
  val ABCD = (ABC >> 1) + D.expand

  val At = A > io.threshold
  val Bt = B > io.threshold
  val Ct = C > io.threshold
  val Dt = D > io.threshold
  val ABt = AB > io.threshold
  val BCt = BC > io.threshold
  val CDt = CD > io.threshold
  val ABCt = ABC > io.threshold
  val BCDt = BCD > io.threshold
  val ABCDt = ABCD > io.threshold

  val TA = io.threshold - A.expand
  val TAB = io.threshold - AB.expand
  val TABC = io.threshold - ABC.expand
  val TABCD = io.threshold - ABCD.expand

  val cmpRes = Vec(At, Bt, Ct, Dt, ABt, BCt, CDt, ABCt, BCDt, ABCDt)
  val subRes = Vec(TA, TAB, TABC, TABCD)
  val addRes = Vec(ABCD, BCD, CD, D)

  io.cmpRes := RegNext(cmpRes)
  (io.addRes, addRes).zipped.foreach((dst, src) => dst := RegNext(src).resized)
  (io.subRes, subRes).zipped.foreach((dst, src) => dst := RegNext(src).resized)
}

object LIF4StepsDynamicPreProcess extends App {
  SpinalVerilog(new LIF4StepsDynamicPreProcess(16))
}