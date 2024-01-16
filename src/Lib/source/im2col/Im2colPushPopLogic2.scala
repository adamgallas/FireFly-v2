package im2col

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Im2colPushPopLogic2(cp: Int, pp: Int, bufferDepth: Int) extends Component {
  val addrWidth = log2Up(bufferDepth)

  val io = new Bundle {
    val pushEvent = slave(Event)
    val popEvent = master(Event)
    val pushPtr = out UInt (addrWidth bits)
    val popPtr = out Vec(UInt(addrWidth bits), pp)
  }

  val pushConfig = new Bundle {
    val channels = in UInt (10 - log2Up(cp) bits)
    val width = in UInt (10 bits)
    val height = in UInt (10 bits)
    val heightReserve = in UInt (4 bits)
  }

  val popConfig = new Bundle {
    val channels = in UInt (10 - log2Up(cp) bits)
    val kernelWidth = in UInt (4 bits)
    val kernelHeight = in UInt (4 bits)
    val width = in UInt (10 - log2Up(pp) bits)
    val height = in UInt (10 bits)

    val kernelWidthAddrInc = in UInt (addrWidth - 2 bits)
    val kernelHeightAddrInc = in UInt (addrWidth bits)
    val widthAddrInc = in UInt (addrWidth - 2 bits)
    val heightAddrInc = in UInt (addrWidth bits)

    val offset = in Vec(UInt(addrWidth - 2 bits), pp)
    val heightInc = in UInt (4 bits)
    val coalesceBound = in UInt (4 bits)
  }

  // cnt and strided addr gen
  val pushGen = new utils.LoopCounter(
    width = List(pushConfig.channels.getWidth, pushConfig.width.getWidth, pushConfig.height.getWidth)
  )
  val popGen = new utils.LoopCntStridedAddrGen(
    boundWidth = List(popConfig.channels.getWidth, popConfig.kernelWidth.getWidth, popConfig.kernelHeight.getWidth, popConfig.width.getWidth, popConfig.height.getWidth),
    incWidth = List(1, popConfig.kernelWidthAddrInc.getWidth, popConfig.kernelHeightAddrInc.getWidth, popConfig.widthAddrInc.getWidth, popConfig.heightAddrInc.getWidth),
    addrWidth = List(8, addrWidth - 2, addrWidth, addrWidth - 2, addrWidth)
  )
  val startPointCache = new utils.StartingPointCache(UInt(addrWidth bits), 8)

  val pushing = io.pushEvent.fire
  val popping = io.popEvent.fire
  val pushFinish = pushing & pushGen.io.cntOvf.reduce(_ & _)
  val popFinish = popping & popGen.io.cntOvf.reduce(_ & _)

  pushGen.io.enable := pushing
  popGen.io.enable := popping
  pushGen.io.bound := Vec(pushConfig.channels, pushConfig.width, pushConfig.height)
  popGen.io.bound := Vec(popConfig.channels, popConfig.kernelWidth, popConfig.kernelHeight, popConfig.width, popConfig.height)
  popGen.io.inc := Vec(U(1), popConfig.kernelWidthAddrInc, popConfig.kernelHeightAddrInc, popConfig.widthAddrInc, popConfig.heightAddrInc)

  startPointCache.io.enableIn := pushFinish
  startPointCache.io.enableOut := popFinish

  // pushPtr
  val pushPtr = Reg(UInt(addrWidth bits)) init (0)
  val pushPtrNext = pushPtr + 1
  when(pushing)(pushPtr := pushPtrNext)
  startPointCache.io.input := pushPtrNext
  io.pushPtr := pushPtr

  // coalesce
  val coalesceCnt = Reg(UInt(4 bits)) init 0
  val coalesces = Reg(UInt(4 bits)) init 0
  val coalesceOvf = coalesceCnt === popConfig.coalesceBound
  when(popping & popGen.io.cntOvf.head) {
    coalesceCnt := coalesceCnt + 1
    when(coalesceOvf) {
      coalesceCnt := 0
      coalesces := coalesces + 1
    }
    when(popGen.io.cntOvf(1)) {
      coalesceCnt.clearAll()
      coalesces.clearAll()
    }
  }

  // popPtr
  val imgAddr = Reg(UInt(addrWidth bits)) init (0)
  val kernelAddr = Reg(UInt(addrWidth bits)) init (0)
  val imgAddrNext = popGen.io.addrNext.takeRight(2)
  val kernelAddrNext = popGen.io.addrNext.dropRight(2)
  when(popping) {
    kernelAddr := kernelAddrNext.reduce(_ + _).resized
    imgAddr := (imgAddrNext.reduce(_ + _) + startPointCache.io.outputNext).resized
  }
  val addrWithReduce = (imgAddr + kernelAddr + coalesces).resized
  val addrWithOs = popConfig.offset.map(_.resize(addrWidth) + addrWithReduce)
  io.popPtr := Vec(addrWithOs.map(_.resized))

  // pushEvent.ready
  val ptrMatch = pushPtr === imgAddr
  val risingOccupancy = RegInit(False)
  val pushing4Real = pushing & pushGen.io.cntOvf.head
  val popping4Real = popping & popGen.io.cntOvf.dropRight(2).reduce(_ & _)
  when(pushing4Real =/= popping4Real) {
    risingOccupancy := pushing4Real
  }
  val full = ptrMatch & risingOccupancy
  io.pushEvent.ready := !full

  // popEvent.valid
  val pushPopFinishCnt = UInt(4 bits) setAsReg() init 0
  pushPopFinishCnt := pushPopFinishCnt + pushFinish.asUInt - popFinish.asUInt
  val popHeightCnt = Reg(UInt(10 bits)) init 0
  when(popping & popGen.io.cntOvf.dropRight(1).reduce(_ & _)) {
    popHeightCnt := popHeightCnt + popConfig.heightInc
    when(popGen.io.cntOvf.last) {
      popHeightCnt.clearAll()
    }
  }

  val heightCond = RegNext(popHeightCnt + pushConfig.heightReserve < pushGen.io.cnt.last, False)
  val popValid = Bool()
  popValid.clear()
  when(pushPopFinishCnt > 0) {
    popValid.set()
  }.otherwise {
    when(pushPopFinishCnt === 0) {
      popValid := heightCond
    }
  }
  io.popEvent.valid := popValid
}
