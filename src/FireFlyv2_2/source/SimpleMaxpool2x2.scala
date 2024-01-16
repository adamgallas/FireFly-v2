import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SimpleMaxpool2x2[T <: Data](dataType: HardType[T], depth: Int, op: (T, T) => T) extends Component {
  val io = new Bundle {
    val input = slave(Flow(Fragment(dataType)))
    val output = master(Flow(Fragment(dataType)))
  }


  val currData = io.input.fragment
  val prevData = RegNextWhen(io.input.fragment, io.input.valid)

  val validToggle = Bool() setAsReg() init false
  validToggle.toggleWhen(io.input.valid)

  val lv1FLow = Flow(Fragment(dataType))
  lv1FLow.valid := io.input.valid & validToggle
  lv1FLow.last := io.input.last
  lv1FLow.fragment := op(currData, prevData)

  val sel = Bool() setAsReg() init false
  val pushEnable = ~sel
  val popEnable = sel
  sel.toggleWhen(io.input.valid & io.input.last)

  val fifo = StreamFifoHighPerf(dataType, depth)
  fifo.io.push.valid := lv1FLow.valid & pushEnable
  fifo.io.push.payload := lv1FLow.fragment
  fifo.io.pop.ready := lv1FLow.valid & popEnable

  io.output.valid := RegNext(lv1FLow.valid & popEnable, init = False)
  io.output.last := RegNext(lv1FLow.last & popEnable, init = False)
  io.output.fragment := RegNext(op(fifo.io.pop.payload, lv1FLow.fragment))
}

