import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class IF4StepsDynamicPostProcess(width: Int) extends Component {

  val io = new Bundle {
    val cmpRes = in Vec(Bool(), 10)
    val subRes = in Vec(SInt(width + 2 bits), 4)
    val addRes = in Vec(SInt(width + 2 bits), 4)
    val outSpikes = out Vec(Bool(), 4)
    val valid = in Bool()
    val last = in Bool()
  }

  def satNegative(src: SInt, m: Int) = {
    val ret = SInt(src.getWidth - m bit)
    ret := src(src.getWidth - m - 1 downto 0)
    when(!src(src.getWidth - 1 downto src.getWidth - m - 1).asBits.andR & src.sign) {
      ret := ret.minValue
    }
    ret
  }

  val prev = SInt(width + 2 bits) setAsReg() init 0
  val prevSpike = Bool() setAsReg() init False

  val ABCD = io.addRes(0)
  val BCD = io.addRes(1)
  val CD = io.addRes(2)
  val D = io.addRes(3)

  val At = io.cmpRes(0)
  val Bt = io.cmpRes(1)
  val Ct = io.cmpRes(2)
  val Dt = io.cmpRes(3)
  val ABt = io.cmpRes(4)
  val BCt = io.cmpRes(5)
  val CDt = io.cmpRes(6)
  val ABCt = io.cmpRes(7)
  val BCDt = io.cmpRes(8)
  val ABCDt = io.cmpRes(9)

  val PAt = prev > io.subRes(0)
  val PABt = prev > io.subRes(1)
  val PABCt = prev > io.subRes(2)
  val PABCDt = prev > io.subRes(3)

  val fireP = Bool()
  val fire0 = Bool()
  val fire1 = Bool()
  val fire2 = Bool()
  val fire3 = Bool()

  fireP := prevSpike

  // fireP  sum
  // 0      PA
  // 1      A
  val sel0 = fireP.asUInt
  val f0List = Vec(PAt, At)
  fire0 := f0List(sel0)

  // fire0  fireP sum
  // 0      0     PAB
  // 0      1     AB
  // 1      0     B
  // 1      1     B
  val sel1 = (fire0 ## fireP).asUInt
  val f1List = Vec(PABt, ABt, Bt, Bt)
  fire1 := f1List(sel1)

  // fire1  fire0  fireP sum
  // 0      0      0     PABC
  // 0      0      1     ABC
  // 0      1      0     BC
  // 0      1      1     BC
  // 1      0      0     C
  // 1      0      1     C
  // 1      1      0     C
  // 1      1      1     C
  val sel2 = (fire1 ## fire0 ## fireP).asUInt
  val f2List = Vec(PABCt, ABCt, BCt, BCt, Ct, Ct, Ct, Ct)
  fire2 := f2List(sel2)

  // fire2  fire1  fire0  fireP sum
  // 0      0      0      0     PABCD
  // 0      0      0      1     ABCD
  // 0      0      1      0     BCD
  // 0      0      1      1     BCD
  // 0      1      0      0     CD
  // 0      1      0      1     CD
  // 0      1      1      0     CD
  // 0      1      1      1     CD
  // 1      0      0      0     D
  // 1      0      0      1     D
  // 1      0      1      0     D
  // 1      0      1      1     D
  // 1      1      0      0     D
  // 1      1      0      1     D
  // 1      1      1      0     D
  // 1      1      1      1     D
  val sel3 = (fire2 ## fire1 ## fire0 ## fireP).asUInt
  val f3List = Vec(PABCDt, ABCDt, BCDt, BCDt, CDt, CDt, CDt, CDt, Dt, Dt, Dt, Dt, Dt, Dt, Dt, Dt)
  fire3 := f3List(sel3)

  val PABCD = (ABCD.expand + prev).sat(1)
  val nextList = Vec(PABCD, ABCD, BCD, BCD, CD, CD, CD, CD, D, D, D, D, D, D, D, D)
  val next = nextList(sel3)

  io.outSpikes := RegNext(Vec(fire0, fire1, fire2, fire3))

  when(io.valid) {
    prev := next
    prevSpike := fire3
    when(io.last) {
      prev.clearAll()
      prevSpike.clear()
    }
  }
}

object IF4StepsDynamicPostProcess extends App {
  SpinalVerilog(new IF4StepsDynamicPostProcess(16))
}