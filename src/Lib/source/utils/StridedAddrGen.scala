package utils

import spinal.core._

import scala.language.postfixOps

object StridedAddrGen {
  def apply(bound: List[UInt], inc: List[UInt], addrWidth: Int, enable: Bool) = {
    require(bound.nonEmpty)
    require(inc.nonEmpty)
    val cnt = bound.map(b => UInt(b.getWidth bits) setAsReg() init 0)
    val addr = Array.fill(inc.length)(UInt(addrWidth bits) setAsReg() init 0)
    val addrNext = Array.fill(inc.length)(UInt(addrWidth bits))
    val cntOvf = (cnt, bound).zipped.map(_ === _)

    (addr, addrNext).zipped.foreach(_ := _)
    (addrNext, addr).zipped.foreach(_ := _)

    when(enable) {
      cnt.head := cnt.head + 1
      addrNext.head := addr.head + inc.head
    }
    for (i <- 1 until bound.length) {
      when(enable && cntOvf.take(i).reduceLeft(_ && _)) {
        cnt(i - 1).clearAll()
        cnt(i) := cnt(i) + 1

        addrNext(i - 1).clearAll()
        addrNext(i) := addr(i) + inc(i)
      }
    }
    when(enable && cntOvf.reduceLeft(_ && _)) {
      cnt.last.clearAll()
      addrNext.last.clearAll()
    }
    (cnt, addr, cntOvf, addrNext)
  }
}
