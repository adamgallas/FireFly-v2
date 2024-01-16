import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SimplePool2x2[T1 <: Data, T2 <: Data, T3 <: Data](
                                                         lv1Type: HardType[T1],
                                                         lv2Type: HardType[T2],
                                                         lv3Type: HardType[T3],
                                                         depth: Int,
                                                         op1: (T1, T1) => T2,
                                                         op2: (T2, T2) => T3
                                                       ) extends Component {
  val io = new Bundle {
    val input = slave(Flow(Fragment(lv1Type)))
    val output = master(Flow(Fragment(lv3Type)))
  }

  val currData = io.input.fragment
  val prevData = RegNextWhen(io.input.fragment, io.input.valid)

  val validToggle = Bool() setAsReg() init false
  validToggle.toggleWhen(io.input.valid)

  val lv1FLow = Flow(Fragment(lv2Type))
  lv1FLow.valid := io.input.valid & validToggle
  lv1FLow.last := io.input.last
  lv1FLow.fragment := op1(currData, prevData)

  val sel = Bool() setAsReg() init false
  val pushEnable = ~sel
  val popEnable = sel
  sel.toggleWhen(io.input.valid & io.input.last)

  val fifo = StreamFifoHighPerf(lv2Type, depth)
  fifo.io.push.valid := lv1FLow.valid & pushEnable
  fifo.io.push.payload := lv1FLow.fragment
  fifo.io.pop.ready := lv1FLow.valid & popEnable

  io.output.valid := RegNext(lv1FLow.valid & popEnable, init = False)
  io.output.last := RegNext(lv1FLow.last & popEnable, init = False)
  io.output.fragment := RegNext(op2(fifo.io.pop.payload, lv1FLow.fragment))
}

