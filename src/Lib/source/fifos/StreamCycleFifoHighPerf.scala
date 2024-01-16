package fifos

import spinal.core._
import spinal.lib._
import spinal.lib.sim.StreamDriver
import utils.LoopCounter

import scala.language.postfixOps

class StreamCycleFifoHighPerf[T <: Data](dataType: HardType[T], depth: Int, reuseWidth: Int = 8) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Stream dataType
    val reuse = in UInt (reuseWidth bits)
    val length = in UInt (log2Up(depth) bits)
  }
  noIoPrefix()

  val popPre = Event
  val ram = Mem(dataType, depth)


  val popping = popPre.fire
  val pushing = io.push.fire

  val popCntGen = new LoopCounter(
    width = List(io.length.getWidth, io.reuse.getWidth)
  )
  popCntGen.io.enable := popping
  popCntGen.io.bound(0) := io.length
  popCntGen.io.bound(1) := io.reuse

  val pushPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val popPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val markStart = UInt(log2Up(depth) bits) setAsReg() init 0
  val markEnd = UInt(log2Up(depth) bits) setAsReg() init 0

  val pushPtrNext = UInt(log2Up(depth) bits)
  val pushPtrPlus = pushPtr + 1
  pushPtr := pushPtrNext
  pushPtrNext := pushPtr

  val popPtrNext = UInt(log2Up(depth) bits)
  val popPtrPlus = popPtr + 1
  popPtr := popPtrNext
  popPtrNext := popPtr

  when(pushing) {
    ram(pushPtr) := io.push.payload
    pushPtrNext := pushPtrPlus
  }

  when(popping) {
    popPtrNext := popPtrPlus
    when(popCntGen.io.cntOvf(0)) {
      popPtrNext := markStart
      when(popCntGen.io.cntOvf(1)) {
        popPtrNext := popPtrPlus
      }
    }
  }

  val endInc = popCntGen.io.cnt(1) === 0
  val startInc = popCntGen.io.cntOvf(1)

  when(popping & endInc) {
    markEnd := popPtrPlus
  }

  when(popping & startInc) {
    markStart := popPtrPlus
  }

  val startRising = RegInit(False)
  when(pushing =/= (popping & startInc)) {
    startRising := pushing
  }

  val endRising = RegInit(False)
  when(pushing =/= (popping & endInc)) {
    endRising := pushing
  }

  val pushMatchEnd = pushPtr === markEnd
  val pushMatchStart = pushPtr === markStart

  val full = Bool()
  val empty = Bool()

  full := pushMatchStart && startRising
  empty := pushMatchEnd && !endRising

  io.push.ready := !full
  popPre.valid := !empty
  io.pop.arbitrationFrom(popPre.m2sPipe())
  io.pop.payload := ram.readSync(popPtr, popPre.ready)
}

object StreamCycleFifoHighPerf extends App {
  SpinalVerilog(new StreamCycleFifoHighPerf(UInt(8 bits), 1024))
}