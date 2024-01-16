package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class StreamInsertDummy[T <: Data](dataType: HardType[T], channels: Int, lenWidth: Int) extends Component {
  val io = new Bundle {
    val inputs = slave(Stream(Fragment(Vec(dataType, channels))))
    val outputs = master(Stream(Fragment(Vec(dataType, channels))))
    val length = in UInt (lenWidth bits)
  }
  noIoPrefix()

  val zero = dataType().getZero
  val validEvent = Event
  validEvent.valid.set()

  val inputFire = io.inputs.fire
  val outputFire = io.outputs.fire

  val elemCnt = Reg(UInt(lenWidth bits)) init 0
  val elemCntNext = elemCnt + 1
  val elemCntOvf = elemCnt === io.length
  when(inputFire) {
    elemCnt := elemCntNext
    when(elemCntOvf) {
      elemCnt.clearAll()
    }
  }

  val data = io.inputs.fragment
  val dataDly = RegNextWhen(data, inputFire)
  val last = elemCntOvf
  val lastDly = Bool() setAsReg()
  lastDly.clearWhen(outputFire).setWhen(last && inputFire)

  val finish = io.inputs.last
  val finishDly = Bool() setAsReg()
  finishDly.clearWhen(outputFire).setWhen(finish && inputFire)
  val concatLeft = dataDly
  val concatRight = data
  val concat = Vec(concatLeft.toList ++ concatRight.toList)

  // 0 0 ^ 1 1 2 2 3 3
  // 0 0 1 1 ^ 2 2 3 3
  // 0 0 1 1 2 2 ^ 3 3
  // 0 0 1 1 2 2 3 3 ^

  // 0 0 1 1 2 2 3 3 -
  // - 0 0 1 1 2 2 3 3

  val cnt = UInt(log2Up(channels) bits) setAsReg() init 0
  val cntDly = UInt(log2Up(channels) bits) setAsReg() init 0
  val sel = Vec(Bool() setAsReg() init False, channels)
  val address = sel.map(s => Mux(s, cntDly, cnt))

  val cond = sel.last || finishDly
  io.outputs.arbitrationFrom(StreamMux(cond.asUInt, Vec(io.inputs.toEvent(), validEvent)))
  io.outputs.last := finishDly

  when(outputFire) {
    cntDly := cnt
    when(last) {
      cnt := cnt + 1
      sel.zipWithIndex.foreach(z => z._1.setWhen(cnt === z._2))
    }
    when(cond) {
      cnt.clearAll()
      cntDly.clearAll()
      sel.foreach(_.clear())
    }
  }

  val ranges = Range(1, channels + 1).map(i => concat.slice(i, channels + i).reverse)
  val selectedData = (ranges, address).zipped.map((seq, addr) => Vec(seq)(addr))
  io.outputs.fragment := Vec(selectedData)
}

object StreamInsertDummy extends App {
  SpinalVerilog(new StreamInsertDummy(Bits(16 bits), 8, 10))
}
