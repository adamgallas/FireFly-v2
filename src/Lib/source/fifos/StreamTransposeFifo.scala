package fifos

import spinal.core._
import spinal.lib._
import utils.{LoopCntStridedAddrGen, LoopCounter, StartingPointCache, StartingPointReg}

import scala.language.postfixOps

class StreamTransposeFifo[T <: Data](dataType: HardType[T], depth: Int, tech: String = "distributed") extends Component {
  val width = log2Up(depth)
  val widthList = List(width, width)

  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(dataType))
    val firstDim = in UInt (width bits)
    val secondDim = in UInt (width bits)
  }

  val ram = Mem(dataType, depth)
  ram.addAttribute("ram_style", tech)


  val pushPtr = UInt(width bits) setAsReg() init 0
  val pushPtrNext = pushPtr + 1
  val pushing = io.push.fire
  when(pushing) {
    ram(pushPtr) := io.push.payload
    pushPtr := pushPtrNext
  }
  val pushGen = new LoopCounter(widthList)
  val pushFinish = pushing & pushGen.io.cntOvf.reduce(_ & _)
  pushGen.io.enable := pushing
  pushGen.io.bound := Vec(io.firstDim, io.secondDim)

  val popPre = Event
  val popping = popPre.fire
  val popGen = new LoopCntStridedAddrGen(widthList, widthList, widthList)
  val popFinish = popping & popGen.io.cntOvf.reduce(_ & _)
  popGen.io.enable := popping
  popGen.io.bound := Vec(io.secondDim, io.firstDim)
  popGen.io.inc := Vec(io.firstDim + 1, U(1))


  val startPointFifo = new StreamFifoLowLatency(UInt(width bits), depth = 32, latency = 1)
  startPointFifo.io.push.valid := pushFinish
  startPointFifo.io.push.payload := pushPtrNext
  startPointFifo.io.pop.ready := popFinish
  val startPoint = UInt(width bits) setAsReg() init 0
  val startPointNext = UInt(width bits)
  startPoint := startPointNext
  startPointNext := startPoint
  when(popFinish) {
    startPointNext := startPointFifo.io.pop.payload
  }

  val popPtr = Reg(UInt(width bits)) init 0
  when(popping) {
    popPtr := startPointNext + popGen.io.addrNext.reduce(_ + _)
  }

  val pushPopFinishCnt = UInt(5 bits) setAsReg() init 0
  pushPopFinishCnt := pushPopFinishCnt + pushFinish.asUInt - popFinish.asUInt
  val popValid = pushPopFinishCnt > 0

  val ptrMatch = pushPtr === startPoint
  val risingOccupancy = RegInit(False)
  val pushing4Real = pushing
  val popping4Real = popping & popGen.io.cntOvf.reduce(_ & _)
  when(pushing4Real =/= popping4Real) {
    risingOccupancy := pushing4Real
  }
  val full = ptrMatch & risingOccupancy
  val pushReady = !full & pushPopFinishCnt < 16

  io.push.ready := pushReady
  popPre.valid := popValid

  io.pop.arbitrationFrom(popPre.m2sPipe())
  io.pop.payload := ram.readSync(popPtr, popPre.ready)
}

object StreamTransposeFifo extends App {
  SpinalVerilog(new StreamTransposeFifo(Vec(Vec(Bits(12 bits), 16), 4), 64))
}