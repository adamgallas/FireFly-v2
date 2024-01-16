package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamAddDummyTail {
  def apply[T <: Data](source: Stream[Fragment[T]], length: UInt) = {

    def addZeroWhen[T_ <: Data](streamIn: Stream[T_], cond: Bool): Stream[T_] = {
      val zero = streamIn.payloadType().getZero
      val next = Stream(streamIn.payloadType)
      next << streamIn
      when(cond) {
        next.payload := zero
        next.valid := True
        streamIn.ready := False
      }
      next
    }

    val ret = Stream(Fragment(source.fragmentType))
    val cnt = UInt(length.getWidth bits) setAsReg() init 0
    val cntOvf = cnt === length - 1
    val flag = Bool() setAsReg() init false
    val noDummy = length === 0
    flag.setWhen(source.fire && source.last && !noDummy)

    when(flag && ret.fire) {
      cnt := cnt + 1
      when(cntOvf) {
        cnt.clearAll()
        flag.clear()
      }
    }

    ret << addZeroWhen(source, flag)
    ret.last.removeAssignments()
    ret.last := Mux(noDummy, source.last, flag && cntOvf)
    ret
  }
}

class StreamAddDummyTail[T <: Data](dataType: HardType[T], width: Int) extends Component {
  val io = new Bundle {
    val push = slave(Stream(Fragment(dataType)))
    val pop = master(Stream(Fragment(dataType)))
    val length = in UInt (width bits)
  }
  noIoPrefix()
  io.pop << StreamAddDummyTail(io.push, io.length)
}
