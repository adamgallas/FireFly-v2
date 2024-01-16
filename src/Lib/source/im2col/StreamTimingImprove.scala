package im2col


import spinal.core._
import spinal.lib._

class StreamTimingImprove[T <: Data](dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(dataType))
  }
  io.pop << io.push.s2mPipe().m2sPipe()
}
