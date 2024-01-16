package systolic.MsVsNsT

import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

case class Int8MsVsNsTMatrixWrapper(ddrClk: ClockDomain, parallelism: (Int, Int, Int, Int)) extends Component {
  val (m, v, n, t) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Bits(t bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
    val outputs = master(Flow(Fragment(Vec(Vec(Bits(18 bits), m), t)))) addTag crossClockDomain
  }
  noIoPrefix()

  def m2sPipe[T <: Data](in: Flow[Fragment[T]]) = {
    val ret = Flow(Fragment(in.fragmentType)) addTag crossClockDomain
    ret.valid := RegNext(in.valid, init = False) addTag crossClockDomain
    ret.last := RegNext(in.last, init = False) addTag crossClockDomain
    ret.fragment := RegNext(in.fragment) addTag crossClockDomain
    ret
  }

  val dataType = HardType(utils.Linked(Vec(Vec(Vec(Bits(t bits), 2), vr), nr), Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), vr), mr)))
  val inputs = Flow(Fragment(Vec(dataType, 2)))

  for (b <- 0 until 2) {
    for (s <- 0 until 2) {
      for (v <- 0 until vr) {
        val index = b * vr * 2 + s * vr + v
        for (i <- 0 until nr) {
          inputs.fragment(b).value(i)(v)(s) := io.inputs.value((n - 1) - i)(index)
        }
        for (i <- 0 until mr) {
          for (j <- 0 until 4) {
            inputs.fragment(b).linked(i)(v)(s)(j) := io.inputs.linked(i * 4 + j)(index)
          }
        }
      }
    }
  }
  inputs.valid := io.inputs.valid
  inputs.last := io.inputs.last && io.inputs.valid
  val inputsPipe = m2sPipe(inputs)

  val ddr = new ClockingArea(ddrClk) {
    val s2t = DataDoubleClockRate(dataType)
    val matrix = Int8MsVsNsTMatrix((mr, vr, nr, t))
    val matrixOutputs = Vec(Vec(Vec(Vec(Flow(Fragment(Vec(Bits(12 bits), 2))), 2), t), mr), nr)
    val align = Array.fill(2)(Array.fill(t)(SystolicAlign(Vec(Bits(24 bits), 2), mr)))

    for (i <- 0 until nr) {
      for (j <- 0 until mr) {
        for (r <- 0 until t) {
          for (s <- 0 until 2) {
            matrixOutputs(i)(j)(r)(s).valid := matrix.io.outputs(i)(j).valid
            matrixOutputs(i)(j)(r)(s).last := matrix.io.outputs(i)(j).last
            matrixOutputs(i)(j)(r)(s).fragment := Vec(matrix.io.outputs(i)(j).fragment(r).slice(s * 2, s * 2 + 2))
          }
        }
      }
    }

    inputsPipe >> s2t.io.inputs
    s2t.io.outputs >> matrix.io.inputs

    val acc = Array.fill(2)(Array.fill(t)(Array.fill(mr)(IntAccChain(nr, 12, compensate = false))))
    for (s <- 0 until 2) {
      for (r <- 0 until t) {
        for (j <- 0 until mr) {
          for (i <- 0 until nr) {
            acc(s)(r)(j).io.inputs(i) << m2sPipe(matrixOutputs(i)(j)(r)(s))
          }
          m2sPipe(acc(s)(r)(j).io.outputs) >> align(s)(r).io.inputs(j)
        }
      }
    }
  }
  val outputs = Flow(Fragment(Vec(Vec(Bits(24 bits), m), t))) addTag crossClockDomain
  outputs.valid := ddr.align.head.head.io.outputs.valid
  outputs.last := ddr.align.head.head.io.outputs.last
  for (r <- 0 until t) {
    for (i <- 0 until mr) {
      for (s <- 0 until 2) {
        for (k <- 0 until 2) {
          val index = i * 4 + s * 2 + k
          outputs.fragment(r)(index) := ddr.align(s)(r).io.outputs.fragment(i)(k)
        }
      }
    }
  }

  val slowDown = new SlowDownBuffer(ddrClk, Vec(Vec(Bits(18 bits), m), t), nr, 32)
  slowDown.io.inputs.valid := outputs.valid
  slowDown.io.inputs.last := outputs.last
  for (i <- 0 until t) {
    for (j <- 0 until m) {
      slowDown.io.inputs.fragment(i)(j) := outputs.fragment(i)(j).asSInt.resize(18).asBits
    }
  }
  slowDown.io.outputs >> io.outputs
}

object Int8MsVsNsTMatrixWrapper {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Int8MsVsNsTMatrixWrapper(ClockDomain.external("ddrClk"), (16, 16, 8, 4)))
  }
}
