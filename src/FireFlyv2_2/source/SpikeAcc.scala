import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SpikeAcc(length: Int) extends Component {
  val io = new Bundle {
    val total = in UInt (8 bits)
    val twoBitAdd = in Bool()
    val input = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
  }

  val isCounting = Bool() setAsReg() init True
  val isOffloading = Bool() setAsReg() init False
  val input = io.input.continueWhen(isCounting)
  input.freeRun()
  io.output.valid := isOffloading

  val inputCnt = UInt(8 bits) setAsReg() init 0
  when(io.input.fire) {
    inputCnt := inputCnt + 1
    when(inputCnt === io.total) {
      inputCnt.clearAll()
      isOffloading.set()
      isCounting.clear()
    }
  }

  val outputCnt = UInt(2 bits) setAsReg() init 0
  when(io.output.fire) {
    outputCnt := outputCnt + 1
    when(outputCnt === 1) {
      outputCnt.clearAll()
      isOffloading.clear()
      isCounting.set()
    }
  }

  val spikeCnt = Vec(UInt(8 bits) setAsReg() init 0, length)
  val low = Vec(spikeCnt.map(_.take(4)))
  val high = Vec(spikeCnt.map(_.drop(4)))
  val sel = Bool() setAsReg() init False
  sel.toggleWhen(io.output.fire)
  io.output.payload := Mux(sel, high, low)

  when(io.input.fire) {
    for (i <- 0 until length) {
      val oneBitSum = CountOne(io.input.payload(i))
      val twoBitSum = io.input.payload(i).take(2).asUInt.expand + io.input.payload(i).drop(2).asUInt.expand
      spikeCnt(i) := spikeCnt(i) + Mux(io.twoBitAdd, twoBitSum, oneBitSum)
    }
  }
  when(io.output.fire) {
    when(outputCnt === 1) {
      for (i <- 0 until length) {
        spikeCnt(i).clearAll()
      }
    }
  }
}

object SpikeAcc extends App {
  SpinalVerilog(new SpikeAcc(16))
}