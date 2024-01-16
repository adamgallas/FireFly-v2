package fifos

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class StreamCycleForever[T <: Data](dataType: HardType[T], bufferDepth: Int) extends Component {
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(Fragment(dataType)))
    val status = in UInt (1 bits)
    val length = in UInt (log2Up(bufferDepth) bits)
  }

  val pushNewData = RegNext(io.status === U(0), init = False)
  val foreverPop = RegNext(io.status === U(1), init = False)

  val ram = Mem(dataType, bufferDepth)
  val pushPtr = UInt(log2Up(bufferDepth) bits) setAsReg() init 0
  val popPtr = UInt(log2Up(bufferDepth) bits) setAsReg() init 0
  val pushPtrOvf = pushPtr === io.length
  val popPtrOvf = popPtr === io.length

  ram.write(pushPtr, io.push.payload, io.push.fire)
  when(io.push.fire) {
    pushPtr := pushPtr + 1
    when(pushPtrOvf) {
      pushPtr.clearAll()
    }
  }

  val readyReg = Bool() setAsReg() init false
  readyReg.setWhen(pushNewData.rise()).clearWhen(pushPtrOvf && io.push.fire)

  io.push.ready := readyReg

  val popPre = Event
  popPre.valid := foreverPop
  when(popPre.fire) {
    popPtr := popPtr + 1
    when(popPtrOvf) {
      popPtr.clearAll()
    }
  }
  io.pop.arbitrationFrom(popPre.m2sPipe())
  io.pop.fragment := ram.readSync(popPtr, popPre.ready)
  io.pop.last := RegNextWhen(popPtrOvf && popPre.fire, popPre.ready, init = False)
}
