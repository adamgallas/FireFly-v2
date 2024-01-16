package im2col

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Im2colPushPopLogic(cp: Int, pp: Int, bufferDepth: Int) extends Component {
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

  val pushing = io.pushEvent.fire
  val popping = io.popEvent.fire

  val cntGen = new Area {

    val cnt4Push = new utils.LoopCounter(List(10, 10))
    val cnt4Pop = new utils.LoopCounter(List(10 - log2Up(pp), 10))
    val cnt4Kernel = new utils.LoopCounter(List(4, 4))

    val pushChCnt = Reg(UInt(10 - log2Up(cp) bits)) init 0
    val popChCnt = Reg(UInt(10 - log2Up(cp) bits)) init 0
    val pushChCntNext = UInt(10 - log2Up(cp) bits)
    val popChCntNext = UInt(10 - log2Up(cp) bits)

    val pushChCntOvf = pushChCnt === pushConfig.channels
    val popChCntOvf = popChCnt === popConfig.channels
    val kernelCntOvf = cnt4Kernel.io.cntOvf.reduce(_ && _)
    val cnt4PushOvf = cnt4Push.io.cntOvf.reduce(_ && _)
    val cnt4PopOvf = cnt4Pop.io.cntOvf.reduce(_ && _)

    pushChCnt := pushChCntNext
    popChCnt := popChCntNext
    pushChCntNext := pushChCnt
    popChCntNext := popChCnt

    when(pushing) {
      pushChCntNext := pushChCnt + 1
      when(pushChCntOvf) {
        pushChCntNext := 0
      }
    }

    when(popping) {
      popChCntNext := popChCnt + 1
      when(popChCntOvf) {
        popChCntNext := 0
      }
    }

    cnt4Push.io.enable := pushing && pushChCntOvf
    cnt4Kernel.io.enable := popping && popChCntOvf
    cnt4Pop.io.enable := popping && popChCntOvf && kernelCntOvf

    cnt4Push.io.bound := Vec(List(pushConfig.width, pushConfig.height))
    cnt4Kernel.io.bound := Vec(List(popConfig.kernelWidth, popConfig.kernelHeight))
    cnt4Pop.io.bound := Vec(List(popConfig.width, popConfig.height))
  }

  val notEmptyGen = new Area {

    import cntGen._

    val currPushFinish = pushing && pushChCntOvf && cnt4PushOvf
    val currPopFinish = popping && popChCntOvf && kernelCntOvf && cnt4PopOvf

    val pushPopFinishCnt = UInt(4 bits) setAsReg() init 0
    pushPopFinishCnt := pushPopFinishCnt + currPushFinish.asUInt - currPopFinish.asUInt

    val startPointfifo = new StreamFifoHighPerf(UInt(addrWidth bits), 32)

    val popHeightCnt = Reg(UInt(10 bits)) init 0
    when(cnt4Pop.io.enable && cnt4Pop.io.cntOvf.head) {
      popHeightCnt := popHeightCnt + popConfig.heightInc
      when(cnt4Pop.io.cntOvf.last) {
        popHeightCnt.clearAll()
      }
    }

    val pushHeight = cnt4Push.io.cnt.last
    val popHeight = popHeightCnt

    val heightCond = popHeight + pushConfig.heightReserve < pushHeight
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

  val pushPtrGen = new Area {

    import cntGen._

    val pushPtr = Reg(UInt(addrWidth bits)) init 0
    val pushPtrNext = pushPtr + 1
    when(pushing) {
      pushPtr := pushPtrNext
    }
    notEmptyGen.startPointfifo.io.push.payload := pushPtrNext
    io.pushPtr := pushPtr
  }

  val popPtrGen = new Area {

    import cntGen._

    val startPoint = UInt(addrWidth bits) setAsReg() init 0
    val startPointNext = UInt(addrWidth bits)
    startPoint := startPointNext
    startPointNext := startPoint

    when(notEmptyGen.currPopFinish) {
      startPointNext := notEmptyGen.startPointfifo.io.pop.payload
    }
    notEmptyGen.startPointfifo.io.push.valid := notEmptyGen.currPushFinish
    notEmptyGen.startPointfifo.io.pop.ready := notEmptyGen.currPopFinish

    val kernelWidthAddr = Reg(UInt(popConfig.kernelWidthAddrInc.getWidth bits)) init 0
    val kernelHeightAddr = Reg(UInt(popConfig.kernelHeightAddrInc.getWidth bits)) init 0
    val kernelWidthAddrNext = UInt(popConfig.kernelWidthAddrInc.getWidth bits)
    val kernelHeightAddrNext = UInt(popConfig.kernelHeightAddrInc.getWidth bits)
    val kernelAddr = Reg(UInt(addrWidth bits)) init 0

    kernelWidthAddr := kernelWidthAddrNext
    kernelHeightAddr := kernelHeightAddrNext
    kernelWidthAddrNext := kernelWidthAddr
    kernelHeightAddrNext := kernelHeightAddr

    when(cnt4Kernel.io.enable) {
      kernelWidthAddrNext := kernelWidthAddr + popConfig.kernelWidthAddrInc
      when(cnt4Kernel.io.cntOvf.head) {
        kernelWidthAddrNext.clearAll()
        kernelHeightAddrNext := kernelHeightAddr + popConfig.kernelHeightAddrInc
        when(cnt4Kernel.io.cntOvf.last) {
          kernelHeightAddrNext.clearAll()
        }
      }
    }

    val widthAddr = Reg(UInt(popConfig.widthAddrInc.getWidth bits)) init 0
    val heightAddr = Reg(UInt(popConfig.heightAddrInc.getWidth bits)) init 0
    val widthAddrNext = UInt(popConfig.widthAddrInc.getWidth bits)
    val heightAddrNext = UInt(popConfig.heightAddrInc.getWidth bits)
    val imgAddr = Reg(UInt(addrWidth bits)) init 0

    widthAddr := widthAddrNext
    heightAddr := heightAddrNext
    widthAddrNext := widthAddr
    heightAddrNext := heightAddr

    when(cnt4Pop.io.enable) {
      widthAddrNext := widthAddr + popConfig.widthAddrInc
      when(cnt4Pop.io.cntOvf.head) {
        widthAddrNext.clearAll()
        heightAddrNext := heightAddr + popConfig.heightAddrInc
        when(cnt4Pop.io.cntOvf.last) {
          heightAddrNext.clearAll()
        }
      }
    }

    when(popping) {
      kernelAddr := (popChCntNext + kernelWidthAddrNext + kernelHeightAddrNext).resized
      imgAddr := (startPointNext + widthAddrNext + heightAddrNext).resized
    }

    val coalesceCnt = Reg(UInt(4 bits)) init 0
    val coalesces = Reg(UInt(4 bits)) init 0
    val coalesceOvf = coalesceCnt === popConfig.coalesceBound
    when(cnt4Kernel.io.enable) {
      coalesceCnt := coalesceCnt + 1
      when(coalesceOvf) {
        coalesceCnt := 0
        coalesces := coalesces + 1
      }
      when(cnt4Kernel.io.cntOvf.head) {
        coalesceCnt.clearAll()
        coalesces.clearAll()
      }
    }

    val addrBase = imgAddr
    val addrReduce = addrBase + kernelAddr
    val addrWithCoalesce = addrReduce + coalesces

    val addr = popConfig.offset.map(_ + addrWithCoalesce)
    io.popPtr := Vec(addr.map(_.resized))
  }

  val notFullGen = new Area {
    val ptrMatch = io.pushPtr === popPtrGen.addrBase
    val risingOccupancy = RegInit(False)
    val pushing4Real = cntGen.cnt4Push.io.enable
    val popping4Real = cntGen.cnt4Pop.io.enable
    when(pushing4Real =/= popping4Real) {
      risingOccupancy := pushing4Real
    }
    val full = ptrMatch & risingOccupancy
    io.pushEvent.ready := !full
  }
}

object Im2colPushPopLogic {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2colPushPopLogic(16, 8, 8192))
  }
}
