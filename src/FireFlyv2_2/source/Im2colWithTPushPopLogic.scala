import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._
import utils.{LoopCntStridedAddrGen, LoopCounter, StartingPointCache}

import scala.language.postfixOps

class Im2colWithTPushPopLogic(cp: Int, pp: Int, bufferDepth: Int) extends Component {
  val addrWidth = log2Up(bufferDepth)

  val io = new Bundle {
    val pushEvent = slave(Event)
    val popEvent = master(Event)
    val pushPtr = out UInt (addrWidth bits)
    val popPtr = out Vec(UInt(addrWidth bits), pp)
  }

  val cfg = new Bundle {
    val timeStep = in UInt (4 bits)

    val pushChannelsTimeStep = in UInt (addrWidth - 4 bits)
    val pushWidth = in UInt (10 bits)
    val pushHeight = in UInt (10 bits)

    val popChannels = in UInt (10 - log2Up(cp) bits)
    val popKernelWidth = in UInt (4 bits)
    val popKernelHeight = in UInt (4 bits)
    val popWidth = in UInt (10 - log2Up(pp) bits)
    val popHeight = in UInt (10 bits)

    val kernelWidthAddrInc = in UInt (addrWidth - 4 bits)
    val kernelHeightAddrInc = in UInt (addrWidth - 1 bits)
    val timeStepAddrInc = in UInt (8 bits)
    val widthAddrInc = in UInt (addrWidth - 4 bits)
    val heightAddrInc = in UInt (addrWidth - 1 bits)

    val heightReserve = in UInt (4 bits)
    val heightInc = in UInt (4 bits)
    val offset = in Vec(UInt(addrWidth - 2 bits), pp)

    val coalesceShape = in UInt (10 bits)
    val coalescePad = in UInt (1 bits)
    val coalesceBound = in UInt (4 bits)

    val popReuse = in UInt (8 bits) default (0)
  }

  import cfg._

  // cnt and strided addr gen
  val pushGen = new LoopCounter(
    width = List(pushChannelsTimeStep.getWidth, pushWidth.getWidth, pushHeight.getWidth)
  )
  val popGen = new LoopCntStridedAddrGen(
    boundWidth = List(popChannels.getWidth, popKernelWidth.getWidth, popKernelHeight.getWidth, timeStep.getWidth, popWidth.getWidth, popHeight.getWidth),
    incWidth = List(1, kernelWidthAddrInc.getWidth, kernelHeightAddrInc.getWidth, timeStepAddrInc.getWidth, widthAddrInc.getWidth, heightAddrInc.getWidth),
    addrWidth = List(8, addrWidth - 2, addrWidth, 10, addrWidth - 2, addrWidth)
  )
  val startPointCache = new StartingPointCache(UInt(addrWidth bits), 32)

  val pushing = io.pushEvent.fire
  val popping = io.popEvent.fire

  val popReuseCnt = UInt(8 bits) setAsReg() init 0
  val singlePassFinish = popping & popGen.io.cntOvf.reduce(_ & _)
  val popReuseOvf = popReuseCnt === popReuse
  when(singlePassFinish) {
    popReuseCnt := popReuseCnt + 1
    when(popReuseOvf) {
      popReuseCnt.clearAll()
    }
  }

  val pushFinish = pushing & pushGen.io.cntOvf.reduce(_ & _)
  //  val popFinish = popping & popGen.io.cntOvf.reduce(_ & _)
  val popFinish = singlePassFinish & popReuseOvf

  pushGen.io.enable := pushing
  popGen.io.enable := popping
  pushGen.io.bound := Vec(pushChannelsTimeStep, pushWidth, pushHeight)
  popGen.io.bound := Vec(popChannels, popKernelWidth, popKernelHeight, timeStep, popWidth, popHeight)
  popGen.io.inc := Vec(U(1), kernelWidthAddrInc, kernelHeightAddrInc, timeStepAddrInc, widthAddrInc, heightAddrInc)

  startPointCache.io.enableIn := pushFinish
  startPointCache.io.enableOut := popFinish

  val pushCoalesceCnt = UInt(10 bits) setAsReg() init 0
  val pushCoalesceOvf = pushCoalesceCnt === coalesceShape
  when(pushing) {
    pushCoalesceCnt := pushCoalesceCnt + 1
    when(pushCoalesceOvf) {
      pushCoalesceCnt.clearAll()
    }
  }

  // pushPtr
  val pushPtr = (UInt(addrWidth bits)).setAsReg().init(0)
  val pushPtrNext = pushPtr + 1 + Mux(pushCoalesceOvf, coalescePad, 0)
  when(pushing)(pushPtr := pushPtrNext)
  startPointCache.io.input := pushPtrNext
  io.pushPtr := pushPtr

  // coalesce
  val coalesceCnt = (UInt(4 bits)).setAsReg().init(0)
  val coalesces = (UInt(4 bits)).setAsReg().init(0)
  val coalesceOvf = coalesceCnt === coalesceBound
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
  val imgAddr = (UInt(addrWidth bits)).setAsReg().init(0)
  val kernelAddr = (UInt(addrWidth bits)).setAsReg().init(0)
  val imgAddrNext = popGen.io.addrNext.takeRight(2)
  val kernelAddrNext = popGen.io.addrNext.dropRight(2)
  when(popping) {
    kernelAddr := kernelAddrNext.reduce(_ + _).resized
    imgAddr := (imgAddrNext.reduce(_ + _) + startPointCache.io.outputNext).resized
  }
  val addrWithReduce = (imgAddr + kernelAddr + coalesces).resized
  val addrWithOs = offset.map(_.resize(addrWidth) + addrWithReduce)
  io.popPtr := Vec(addrWithOs.map(_.resized))

  // pushEvent.ready
  val ptrMatch = pushPtr <= imgAddr && imgAddr <= pushPtrNext
  val risingOccupancy = RegInit(False)
  val pushing4Real = pushing & pushGen.io.cntOvf.head
  val popping4Real = popping & popGen.io.cntOvf.dropRight(2).reduce(_ & _)
  when(pushing4Real =/= popping4Real) {
    risingOccupancy := pushing4Real
  }
  val full = ptrMatch & risingOccupancy
  io.pushEvent.ready := !full

  // popEvent.valid
  val pushPopFinishCnt = UInt(6 bits) setAsReg() init 0
  pushPopFinishCnt := pushPopFinishCnt + pushFinish.asUInt - popFinish.asUInt
  val popHeightCnt = (UInt(10 bits)).setAsReg().init(0)
  when(popping & popGen.io.cntOvf.dropRight(1).reduce(_ & _)) {
    popHeightCnt := popHeightCnt + heightInc
    when(popGen.io.cntOvf.last) {
      popHeightCnt.clearAll()
    }
  }

  //  val heightCond = RegNext(popHeightCnt + heightReserve < pushGen.io.cnt.last, init = False)
  val heightCond = popHeightCnt + heightReserve < pushGen.io.cnt.last
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

  //  val expose = new Bundle {
  //    val pushing = out Bool()
  //    val pushFinish = out Bool()
  //    val pushCnt0 = out UInt()
  //    val pushCnt1 = out UInt()
  //    val pushCnt2 = out UInt()
  //    val cntOvf = out Vec(Bool(), 3)
  //    val bound0 = out UInt()
  //    val bound1 = out UInt()
  //    val bound2 = out UInt()
  //  }
  //  expose.pushing := RegNext(pushing, init = False)
  //  expose.pushFinish := RegNext(pushFinish, init = False)
  //  expose.pushCnt0 := RegNext(pushGen.io.cnt(0))
  //  expose.pushCnt1 := RegNext(pushGen.io.cnt(1))
  //  expose.pushCnt2 := RegNext(pushGen.io.cnt(2))
  //  expose.cntOvf := RegNext(pushGen.io.cntOvf)
  //  expose.bound0 := RegNext(pushGen.io.bound(0))
  //  expose.bound1 := RegNext(pushGen.io.bound(1))
  //  expose.bound2 := RegNext(pushGen.io.bound(2))
}

object Im2colWithTPushPopLogic extends App {
  SpinalVerilog(new Im2colWithTPushPopLogic(16, 8, 8192))
}