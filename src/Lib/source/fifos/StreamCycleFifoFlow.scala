package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamCycleFifoFlow {
  def apply[T <: Data](dataType: T, depth: Int) = new StreamCycleFifoFlow(dataType, depth)
}

class StreamCycleFifoFlow[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Flow dataType
    val reuse = in UInt (8 bits)
    val length = in UInt (log2Up(depth) bits)
    val flush = in Bool() default False

    val reuseCounter = out UInt (8 bits)
    val lengthCounter = out UInt (log2Up(depth) bits)
  }
  noIoPrefix()

  val ram = Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val markPtr = UInt(log2Up(depth) bits) setAsReg() init 0
  val reuseCnt = Counter(8 bits)
  val lenCnt = Counter(depth)

  val lenCntEquBound = lenCnt.value === io.length
  val reuseCntEquBound = reuseCnt.value === io.reuse
  val reuseCntEquBoundMinus1 = reuseCnt.value === io.reuse - 1

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
  io.pop.valid := RegNext(!empty, init = False)
  io.pop.payload := ram.readSync(popPtr)

  val popping = !empty
  val pushing = io.push.fire

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
      when(reuseCntEquBoundMinus1) {
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
}