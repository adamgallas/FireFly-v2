package utils

import spinal.core._

import scala.language.postfixOps

class LoopCntStridedAddrGen(boundWidth: List[Int], incWidth: List[Int], addrWidth: List[Int]) extends Component {
  val length = boundWidth.length
  require(incWidth.length == length)
  require(addrWidth.length == length)
  val io = new Bundle {
    val bound = in Vec boundWidth.map(b => UInt(b bits))
    val inc = in Vec incWidth.map(i => UInt(i bits))
    val enable = in Bool()

    val cnt = out Vec boundWidth.map(b => UInt(b bits))
    val cntOvf = out Vec(Bool(), length)
    val addr = out Vec addrWidth.map(w => UInt(w bits))
    val addrNext = out Vec addrWidth.map(w => UInt(w bits))
  }

  val cnt = boundWidth.map(b => UInt(b bits) setAsReg() init 0)
  val addr = addrWidth.map(w => UInt(w bits) setAsReg() init 0)
  val addrNext = addrWidth.map(w => UInt(w bits))
  val cntOvf = (cnt, io.bound).zipped.map(_ === _)

  (addr, addrNext).zipped.foreach(_ := _)
  (addrNext, addr).zipped.foreach(_ := _)

  when(io.enable) {
    cnt.head := cnt.head + 1
    addrNext.head := addr.head + io.inc.head
  }
  for (i <- 1 until length) {
    when(io.enable && cntOvf.take(i).reduceLeft(_ && _)) {
      cnt(i - 1).clearAll()
      cnt(i) := cnt(i) + 1

      addrNext(i - 1).clearAll()
      addrNext(i) := addr(i) + io.inc(i)
    }
  }
  when(io.enable && cntOvf.reduceLeft(_ && _)) {
    cnt.last.clearAll()
    addrNext.last.clearAll()
  }

  io.cnt := Vec(cnt)
  io.cntOvf := Vec(cntOvf)
  io.addr := Vec(addr)
  io.addrNext := Vec(addrNext)

//  val cnt = boundWidth.map(b => UInt(b bits) setAsReg() init 0)
//  val addr = addrWidth.map(w => UInt(w bits) setAsReg() init 0)
//  val addrNext = addrWidth.map(w => UInt(w bits))
//
//  val boundIsZero = io.bound.map(_ === 0)
//  val cntOvfLogic = (cnt, io.bound).zipped.map(_ === _ - 1)
//  val cntOvf = Vec(Reg(Bool()) init False, io.bound.length)
//
//  val inc = Vec(Bool(), io.bound.length + 1)
//  inc.head := io.enable
//  for (i <- 1 until inc.length) {
//    inc(i) := io.enable && cntOvf.take(i).reduceLeft(_ && _)
//  }
//  for (i <- io.bound.indices) {
//    cntOvf(i).clearWhen(inc(i)).setWhen(boundIsZero(i) || inc(i) & cntOvfLogic(i))
//  }
//
//  (addr, addrNext).zipped.foreach(_ := _)
//  (addrNext, addr).zipped.foreach(_ := _)
//
//  when(inc.head) {
//    cnt.head := cnt.head + 1
//    addrNext.head := addr.head + io.inc.head
//  }
//  for (i <- 1 until length) {
//    when(inc(i)) {
//      cnt(i - 1).clearAll()
//      cnt(i) := cnt(i) + 1
//
//      addrNext(i - 1).clearAll()
//      addrNext(i) := addr(i) + io.inc(i)
//    }
//  }
//  when(inc.last) {
//    cnt.last.clearAll()
//    addrNext.last.clearAll()
//  }
//
//  io.cnt := Vec(cnt)
//  io.cntOvf := Vec(cntOvf)
//  io.addr := Vec(addr)
//  io.addrNext := Vec(addrNext)


}

object LoopCntStridedAddrGen extends App{
  SpinalVerilog(new LoopCntStridedAddrGen(List(4, 4, 4), List(8, 8, 8), List(10, 10, 10)))
}