package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamFifoHighPerfAsync {
  def apply[T <: Data](dataType: HardType[T], depth: Int) = new StreamFifoHighPerfAsync(dataType, depth)
}

class StreamFifoHighPerfAsync[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Stream dataType
    val flush = in Bool() default False
  }
  noIoPrefix()

  val ram = Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val pushing = io.push.fire
  val popping = io.pop.fire
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  io.push.ready := !full
  io.pop.valid := !empty
  io.pop.payload := ram.readAsync(popPtr)

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
}
