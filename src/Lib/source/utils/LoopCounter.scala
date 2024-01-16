package utils

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

object LoopCounter {
  def apply(bound: List[UInt], enable: Bool) = {
    require(bound.nonEmpty)
    val cnt = bound.map(b => UInt(b.getWidth bits) setAsReg() init 0)
    val cntOvf = (cnt, bound).zipped.map(_ === _)
    when(enable)(cnt.head := cnt.head + 1)
    for (i <- 1 until bound.length) {
      when(enable && cntOvf.take(i).reduceLeft(_ && _)) {
        cnt(i - 1).clearAll()
        cnt(i) := cnt(i) + 1
      }
    }
    when(enable && cntOvf.reduceLeft(_ && _))(cnt.last.clearAll())
    (cnt, cntOvf)
  }

  def highPerf(bound: List[UInt], enable: Bool) = {
    require(bound.nonEmpty)
    val cnt = bound.map(b => UInt(b.getWidth bits) setAsReg() init 0)

    val boundIsZero = bound.map(_ === 0)
    val boundIsZeroDly = boundIsZero.map(s => RegNext(s, init = False))
    val boundSwitch = (boundIsZero, boundIsZeroDly).zipped.map(_ =/= _)

    val cntOvfLogic = (cnt, bound).zipped.map(_ === _ - 1)
    val cntOvf = Vec(Reg(Bool()) init False, bound.length)

    val inc = Vec(Bool(), bound.length + 1)
    inc.head := enable
    for (i <- 1 until inc.length) {
      inc(i) := enable && cntOvf.take(i).reduceLeft(_ && _)
    }

    for (i <- bound.indices) {
      cntOvf(i).clearWhen(inc(i)).setWhen(boundIsZero(i) || inc(i) & cntOvfLogic(i))
      cntOvf(i).clearWhen(boundSwitch(i) & ~boundIsZero(i))
    }

    when(inc.head)(cnt.head := cnt.head + 1)
    for (i <- 1 until bound.length) {
      when(inc(i)) {
        cnt(i - 1).clearAll()
        cnt(i) := cnt(i) + 1
      }
    }
    when(inc.last)(cnt.last.clearAll())
    (cnt, cntOvf)
  }

  def constant(bound: List[Int], enable: Bool) = {
    require(bound.nonEmpty)
    val cnt = bound.map(b => Counter(b))
    val cntOvf = cnt.map(_.willOverflowIfInc)
    when(enable)(cnt.head.increment())
    for (i <- 1 until bound.length) {
      when(enable && cntOvf.take(i).reduceLeft(_ && _)) {
        cnt(i - 1).clear()
        cnt(i).increment()
      }
    }
    when(enable && cntOvf.reduceLeft(_ && _))(cnt.last.clearAll())
    (cnt.map(_.value), cntOvf)
  }
}

case class LoopCounter(width: List[Int]) extends Component {
  val io = new Bundle {
    val enable = in Bool()
    val bound = in Vec width.map(w => UInt(w bits))
    val cnt = out Vec width.map(w => UInt(w bits))
    val cntOvf = out Vec(Bool(), width.length)
  }
  noIoPrefix()
  val (cnt, cntOvf) = LoopCounter.highPerf(io.bound.toList, io.enable)
  io.cnt := Vec(cnt)
  io.cntOvf := Vec(cntOvf)
}
