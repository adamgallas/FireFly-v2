package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamAddDummyHeader {
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
    val cntOvf = cnt === length
    when(ret.fire && !cntOvf)(cnt := cnt + 1)
    when(ret.fire && ret.last)(cnt.clearAll())

    ret << addZeroWhen(source, source.valid && !cntOvf)
    ret
  }
}

class StreamAddDummyHeader[T <: Data](dataType: HardType[T], width: Int) extends Component {
  val io = new Bundle {
    val push = slave(Stream(Fragment(dataType)))
    val pop = master(Stream(Fragment(dataType)))
    val length = in UInt (width bits)
  }
  noIoPrefix()
  io.pop << StreamAddDummyHeader(io.push, io.length)
}
