package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamBigWidthAdapter {
  def apply[T <: Data](input: Stream[T], output: Stream[Vec[T]], factor: Int): Unit = {
    val cnt = Counter(factor, inc = input.fire)
    val buf = Reg(Vec(input.payloadType, factor - 1))
    when(input.fire) {
      buf.last := input.payload
      for (i <- 0 until buf.length - 1) buf(i) := buf(i + 1)
    }
    input.ready := !(!output.ready && cnt.willOverflowIfInc)
    output.valid := input.valid && cnt.willOverflowIfInc
    output.payload(factor - 1) := input.payload
    for (i <- 0 until factor - 1) output.payload(i) := buf(i)
  }

  def apply[T <: Data](input: Stream[T], factor: Int) = {
    val output = Stream(Vec(input.payloadType, factor))
    val cnt = Counter(factor, inc = input.fire)
    val buf = Reg(Vec(input.payloadType, factor - 1))
    when(input.fire) {
      buf.last := input.payload
      for (i <- 0 until buf.length - 1) buf(i) := buf(i + 1)
    }
    input.ready := !(!output.ready && cnt.willOverflowIfInc)
    output.valid := input.valid && cnt.willOverflowIfInc
    output.payload(factor - 1) := input.payload
    for (i <- 0 until factor - 1) output.payload(i) := buf(i)
    output
  }

  //  def highPerf[T <: Data](input: Stream[T], factor: Int, group: Int, occupancy: UInt, slack: Int) = {
  //    val output = Stream(Vec(input.payloadType, factor))
  //    val cnt = Counter(factor)
  //    val enoughData = occupancy >= slack
  //    val cond_0 = Bool() setAsReg() init false
  //    val cond_1 = Bool() setAsReg() init true
  //    val cond = cond_0 & cond_1
  //    val overflow = cnt.willOverflow
  //    cond_0.setWhen(enoughData).clearWhen(overflow)
  //    cond_1.setWhen(output.ready).clearWhen(overflow)
  //    when(cond)(cnt.increment())
  //
  //    val condReg = RegNext(cond, init = False)
  //    val overflowReg = RegNext(overflow, init = False)
  //    val condReplicate = for (i <- 0 until group) yield RegNext(cond, init = False)
  //    condReplicate.foreach(r => r.addAttribute("KEEP", "TRUE"))
  //
  //    val srl = new HistoryGroup(input.payloadType, group, factor)
  //    (srl.io.enable, condReplicate).zipped.foreach(_ := _)
  //    (output.payload, srl.io.outputs).zipped.foreach(_ := _)
  //    srl.io.input := input.payload
  //
  //    val valid = Bool() setAsReg() init false
  //    valid.setWhen(overflowReg).clearWhen(output.ready)
  //
  //    output.valid := valid
  //    input.ready := condReg
  //    output
  //  }

  def highPerf[T <: Data](input: Stream[T], factor: Int, group: Int) = {
    val output = Stream(Vec(input.payloadType, factor))
    val inputFire = input.fire

    val cnt = Counter(factor)
    val overflow = cnt.willOverflow
    val overflowReg = RegNext(overflow, init = False)
    when(inputFire)(cnt.increment())

    val startCond = Bool() setAsReg() init true
    startCond.setWhen(output.ready).clearWhen(overflow)
    input.ready := startCond

    val validData = RegNext(input.payload)
    val valids = for (i <- 0 until group) yield RegNext(inputFire, init = False)
    valids.foreach(r => r.addAttribute("KEEP", "TRUE"))

    val srl = new HistoryGroup(input.payloadType, group, factor)
    (srl.io.enable, valids).zipped.foreach(_ := _)
    (output.payload, srl.io.outputs).zipped.foreach(_ := _)
    srl.io.input := validData

    val outputValid = Bool() setAsReg() init false
    outputValid.setWhen(overflowReg).clearWhen(output.ready)
    output.valid := outputValid
    output
  }

}
