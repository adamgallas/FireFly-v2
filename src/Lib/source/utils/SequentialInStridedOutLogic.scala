package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SequentialInStridedOutLogic(
                                   bank: Int,
                                   pushCntWidth: List[Int],
                                   popCntWidth: List[Int],
                                   strideIncWidth: List[Int],
                                   strideAddrWidth: List[Int],
                                   splitAt: Int,
                                   addrWidth: Int,
                                   offsetWidth: Int
                                 ) extends Component {

  val io = new Bundle {
    val pushing = in Bool()
    val popping = in Bool()
    val pushPtr = out UInt (addrWidth - log2Up(bank) bits)
    val popPtr = out Vec(UInt(addrWidth bits), bank)
    val pushCnt = out Vec pushCntWidth.map(b => UInt(b bits))
    val popCnt = out Vec popCntWidth.map(b => UInt(b bits))
  }

  val config = new Bundle {
    val pushCntBound = in Vec pushCntWidth.map(b => UInt(b bits))
    val popCntBound = in Vec popCntWidth.map(b => UInt(b bits))
    val popStrideInc = in Vec strideIncWidth.map(i => UInt(i bits))
    val popBankOffsets = in Vec(UInt(offsetWidth bits), bank - 1)
  }

  val pushGen = new LoopCounter(pushCntWidth)
  val popGen = new LoopCntStridedAddrGen(popCntWidth, strideIncWidth, strideAddrWidth)
  val startPoint = new StartingPointCache(UInt(addrWidth bits), 8)

  val pushing = Bool()
  val popping = Bool()
  pushing := io.pushing
  popping := io.popping

  val pushFinish = pushing && pushGen.io.cntOvf.reduce(_ & _)
  val popFinish = popping && popGen.io.cntOvf.reduce(_ & _)

  pushGen.io.enable := pushing
  pushGen.io.bound := config.pushCntBound

  popGen.io.enable := popping
  popGen.io.bound := config.popCntBound
  popGen.io.inc := config.popStrideInc

  startPoint.io.enableIn := pushFinish
  startPoint.io.enableOut := popFinish

  val pushPtr = Reg(UInt(addrWidth - log2Up(bank) bits)) init (0)
  val pushPtrNext = pushPtr + 1
  when(pushing)(pushPtr := pushPtrNext)
  startPoint.io.input := pushPtrNext
  io.pushPtr := pushPtr

  val res = popGen.io.addrNext.take(splitAt)
  val base = popGen.io.addrNext.drop(splitAt)
  val offset = List(U(0, addrWidth bits)) ++ config.popBankOffsets
  val resAddr = UInt(addrWidth bits) setAsReg() init 0
  val baseAddr = UInt(addrWidth bits) setAsReg() init 0
  when(popping) {
    resAddr := res.reduce(_ + _).resized
    baseAddr := base.reduce(_ + _).resized + startPoint.io.outputNext
  }

  val addr = resAddr + baseAddr
  io.popPtr := Vec(offset.map(_ + addr))

  io.pushCnt := pushGen.io.cnt
  io.popCnt := popGen.io.cnt
}
