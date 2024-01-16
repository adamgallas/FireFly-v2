import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class FragmentConcat[T <: Data](dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val x1Input = slave(Flow(Fragment(dataType)))
    val x2Input = slave(Flow(Fragment(Vec(dataType, 2))))

    val x2Output = master(Flow(Fragment(Vec(dataType, 2))))
    val x4Output = master(Flow(Fragment(Vec(dataType, 4))))

    val isX1Input = in Bool()
    val isX2Output = in Bool()
  }

  val inputValid = Mux(io.isX1Input, io.x1Input.valid, io.x2Input.valid)
  val inputLast = Mux(io.isX1Input, io.x1Input.last, io.x2Input.last)
  val fragmentEnd = inputValid & inputLast

  val is1to4 = io.isX1Input & ~io.isX2Output
  val is1to2 = io.isX1Input & io.isX2Output
  val is2to4 = ~io.isX1Input & ~io.isX2Output

  val isUpsize4 = is1to4
  val isUpsize2 = ~is1to4

  val fragment2th = Bool() setAsReg() init false
  val fragment4th = Bool() setAsReg() init false
  val lastFragment = Mux(isUpsize4, fragment4th, fragment2th)

  fragment2th.toggleWhen(fragmentEnd & isUpsize2)
  val fragmentCnt = UInt(2 bits) setAsReg() init 0
  when(fragmentEnd & isUpsize4) {
    fragmentCnt := fragmentCnt + 1
    when(fragmentCnt === 2) {
      fragment4th.set()
    }
    when(fragmentCnt === 3) {
      fragment4th.clear()
      fragmentCnt.clearAll()
    }
  }

  val firstFragment2th = ~fragment2th
  val firstFragment4th = fragmentCnt === 0
  val firstFragment = Mux(isUpsize4, firstFragment4th, firstFragment2th)

  val fifo = StreamFifoHighPerf(Vec(dataType, 3), 32)
  val pushValid = inputValid & ~lastFragment
  val popReady = inputValid & ~firstFragment
  fifo.io.push.valid := pushValid
  fifo.io.pop.ready := popReady

  val x1toFifo = Vec(io.x1Input.fragment, fifo.io.pop.payload(0))
  val x2toFifo = io.x2Input.fragment
  val fifoInput = Vec(Mux(io.isX1Input, x1toFifo, x2toFifo) ++ Vec(fifo.io.pop.payload(1)))
  fifo.io.push.payload := fifoInput

  io.x2Output.valid := inputValid & lastFragment & io.isX2Output
  io.x4Output.valid := inputValid & lastFragment & ~io.isX2Output
  io.x2Output.last := inputLast & lastFragment & io.isX2Output
  io.x4Output.last := inputLast & lastFragment & ~io.isX2Output

  val x2tox4 = Vec(fifo.io.pop.payload.take(2) ++ io.x2Input.fragment)
  val x1tox4 = Vec((Vec(io.x1Input.fragment) ++ fifo.io.pop.payload).reverse)

  io.x2Output.fragment := Vec(fifo.io.pop.payload(0), io.x1Input.fragment)
  io.x4Output.fragment := Mux(io.isX1Input, x1tox4, x2tox4)
}

object FragmentConcat extends App {
  SpinalVerilog(new FragmentConcat(UInt(16 bits)))
}