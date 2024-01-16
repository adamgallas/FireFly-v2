package fifos

import spinal.core._
import spinal.lib._
import spinal.lib.sim.StreamDriver

import scala.language.postfixOps

object StreamCycleFifo {
  def apply[T <: Data](dataType: T, depth: Int) = new StreamCycleFifo(dataType, depth)
}

class StreamCycleFifo[T <: Data](dataType: HardType[T], depth: Int, reuseWidth: Int = 8) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Stream dataType
    val reuse = in UInt (reuseWidth bits)
    val length = in UInt (log2Up(depth) bits)
    val flush = in Bool() default False

    val reuseCounter = out UInt (reuseWidth bits)
    val lengthCounter = out UInt (log2Up(depth) bits)
//    val occupancy = out UInt (log2Up(depth + 1) bits)
//    val availability = out UInt (log2Up(depth + 1) bits)
  }
  noIoPrefix()

  val popPre = Event

  val ram = Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val markPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val reuseCnt = Counter(reuseWidth bits)
  val lenCnt = Counter(depth)

  val popping = popPre.fire
  val pushing = io.push.fire

  val lenCntEquBound = lenCnt.value === io.length
  val reuseCntEquBound = reuseCnt.value === io.reuse
  val reuseCntEquBoundPre = reuseCnt.value === io.reuse - 1 || io.reuse === 0

  val lenCntEquZero = Bool() setAsReg() init true
  val reuseCntEquZero = Bool() setAsReg() init true
  val phase1 = Bool() setAsReg() init true
  val phase2 = Bool() setAsReg() init false
  val phase3 = Bool() setAsReg() init false

  val popPtrNext = Mux(popPtr === U(depth - 1), U(0), popPtr + 1)

  val risingOccupancy = RegInit(False)
  val pushMatchMark = pushPtr.value === markPtr
  val pushMatchPop = pushPtr.value === popPtr

  val phase1Full = phase1 && pushMatchMark && Mux(lenCntEquZero, risingOccupancy, True)
  val phase2Full = phase2 && pushMatchMark
  val phase3Full = phase3 && pushMatchPop

  val phase1Empty = phase1 && pushMatchPop && Mux(lenCntEquZero, !risingOccupancy, True)
  val phase2Empty = False
  val phase3Empty = False

  val full = phase1Full || phase2Full || phase3Full
  val empty = phase1Empty || phase2Empty || phase3Empty

  io.push.ready := !full
  popPre.valid := !empty
  io.pop.arbitrationFrom(popPre.m2sPipe(flush = io.flush))
  io.pop.payload := ram.readSync(popPtr, popPre.ready)

  when(pushing =/= popping) {
    risingOccupancy := pushing
  }

  when(pushing) {
    ram(pushPtr.value) := io.push.payload
    pushPtr.increment()
  }

  when(popping) {
    lenCnt.increment()
    lenCntEquZero.clear()
    popPtr := popPtrNext
    when(lenCntEquBound) {
      reuseCnt.increment()
      lenCnt.clear()
      lenCntEquZero.set()
      reuseCntEquZero.clear()
      popPtr := markPtr

      when(reuseCntEquZero) {
        phase1.clear()
        phase2.set()
      }
      when(reuseCntEquBoundPre) {
        phase2.clear()
        phase3.set()
      }
      when(reuseCntEquBound) {
        phase3.clear()
        phase1.set()

        popPtr := popPtrNext
        markPtr := popPtrNext
        reuseCnt.clear()
        reuseCntEquZero.set()
      }
    }
  }

  when(io.flush) {
    pushPtr.clear()
    popPtr.clearAll()
    lenCnt.clear()
    reuseCnt.clear()
  }

  io.reuseCounter := reuseCnt.value
  io.lengthCounter := lenCnt.value
//
//  val ptrDif = pushPtr - popPtr
//  io.occupancy := ((risingOccupancy && pushMatchPop) ## ptrDif).asUInt
//  io.availability := ((!risingOccupancy && pushMatchPop) ## (popPtr - pushPtr)).asUInt

}