import fifos.StreamFifoHighPerf
import im2col.TensorThrowing
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ConcatInterleaver[T <: Data](dataType: HardType[T], width: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave Stream dataType, 2)
    val outputs = master Stream dataType
    val length = in Vec(UInt(width bits), 2)
  }

  val cnt = UInt(width + 1 bits) setAsReg() init 0
  val sel = Mux(cnt < io.length(0), U(0), U(1))
  val overflow = cnt === io.length(0) + io.length(1) - 1

  when(io.outputs.fire) {
    cnt := cnt + 1
    when(overflow) {
      cnt.clearAll()
    }
  }

  io.outputs << StreamMux(sel, io.inputs)
}