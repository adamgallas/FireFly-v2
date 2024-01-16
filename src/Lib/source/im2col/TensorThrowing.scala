package im2col

import spinal.core._
import spinal.lib._
import utils.LoopCounter

import scala.language.postfixOps

class TensorThrowing[T <: Data](dataType: HardType[T], widthOfCnt: List[Int]) extends Component {
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(dataType))
  }
  val config = new Bundle {
    val shape = in Vec widthOfCnt.map(w => UInt(w bits))
    val throwAtHead = in Vec widthOfCnt.map(w => UInt(w bits))
    val throwAtTail = in Vec widthOfCnt.map(w => UInt(w bits))
  }
  noIoPrefix()
  config.setName("")

  val dummy = Stream(dataType)
  val dummyValid = Bool() setAsReg() init False
  dummy.payload := dataType().getZero
  dummy.valid := dummyValid

  val throwCond = Bool()
  io.pop << io.push.throwWhen(throwCond)

  val pushing = io.push.fire
  val popping = io.pop.fire

  val left = config.throwAtHead
  val right = (config.throwAtHead, config.shape).zipped.map(_ + _)
  val total = (right, config.throwAtTail).zipped.map(_ + _ -1)

  val loopCnt = new LoopCounter(widthOfCnt)
  val cnt = loopCnt.io.cnt
  val cntOvf = loopCnt.io.cntOvf
  loopCnt.io.enable := popping
  loopCnt.io.bound := Vec(total)

  val leftCond = (left, cnt).zipped.map(_ <= _)
  val rightCond = (cnt, right).zipped.map(_ < _)
  val cond = (leftCond, rightCond).zipped.map(_ && _)
  throwCond := ~cond.reduce(_ && _)

  dummyValid.setWhen(io.push.valid)
  dummyValid.clearWhen(cntOvf.reduce(_ && _) && popping)
}

object TensorThrowing {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TensorThrowing(Bits(32 bits), List(10, 10, 8)))
  }
}
