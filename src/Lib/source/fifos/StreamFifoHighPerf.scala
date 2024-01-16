package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamFifoHighPerf {
  def apply[T <: Data](dataType: HardType[T], depth: Int) = new StreamFifoHighPerf(dataType, depth)
}

class StreamFifoHighPerf[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Stream dataType
    val flush = in Bool() default False
    val occupancy = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }
  noIoPrefix()
  val popPre = Event

  val ram = Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val pushing = io.push.fire
  val popping = popPre.fire
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

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
    popPtr.increment()
  }

  when(io.flush) {
    pushPtr.clear()
    popPtr.clear()
    risingOccupancy := False
  }

  val ptrDif = pushPtr - popPtr
  io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).asUInt
  io.availability := ((!risingOccupancy && ptrMatch) ## (popPtr - pushPtr)).asUInt

  //  val popEvent = Event
  //  val ram = Mem(dataType, depth)
  //  val pushPtr = Counter(depth)
  //  val popPtr = Counter(depth)
  //  val ptrMatch = pushPtr === popPtr
  //  val risingOccupancy = RegInit(False)
  //  val pushing = io.push.fire
  //  val popping = popEvent.fire
  //  val empty = ptrMatch & !risingOccupancy
  //  val full = ptrMatch & risingOccupancy
  //
  //  io.push.ready := !full
  //  popEvent.valid := !empty & !(RegNext(popPtr.valueNext === pushPtr, False) & !full) //mem write to read propagation
  //
  //  val rValid = RegNextWhen(popEvent.valid, popEvent.ready) init False
  //  val rData = ram.readSync(popPtr, popEvent.ready)
  //  popEvent.ready := io.pop.ready
  //  popEvent.ready setWhen (!rValid)
  //  io.pop.valid := rValid
  //  io.pop.payload := rData
  //
  //  when(pushing =/= popping) {
  //    risingOccupancy := pushing
  //  }
  //  when(pushing) {
  //    ram(pushPtr.value) := io.push.payload
  //    pushPtr.increment()
  //  }
  //  when(popping) {
  //    popPtr.increment()
  //  }
  //
  //  when(io.flush) {
  //    rValid.clear()
  //    pushPtr.clear()
  //    popPtr.clear()
  //    risingOccupancy := False
  //  }
}