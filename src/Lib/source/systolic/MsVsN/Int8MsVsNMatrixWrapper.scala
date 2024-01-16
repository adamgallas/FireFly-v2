package systolic.MsVsN

import spinal.core._
import spinal.lib._
import systolic._
import xilinx.DSP48E2._
import scala.language.postfixOps

case class Int8MsVsNMatrixWrapper(ddrClk: ClockDomain, parallelism: (Int, Int, Int)) extends Component {
  val (m, v, n) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Bits(1 bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), m)))) addTag crossClockDomain
  }
  noIoPrefix()

  def m2sPipe[T <: Data](in: Flow[Fragment[T]]) = {
    val ret = Flow(Fragment(in.fragmentType)) addTag crossClockDomain
    ret.valid := RegNext(in.valid, init = False) addTag crossClockDomain
    ret.last := RegNext(in.last, init = False) addTag crossClockDomain
    ret.fragment := RegNext(in.fragment) addTag crossClockDomain
    ret
  }

  val dataType = HardType(utils.Linked(Vec(Vec(Vec(Bits(1 bits), 2), vr), nr), Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), vr), mr)))
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
    val matrix = Int8MsVsNMatrix((mr, vr, nr))
    val matrixOutputs = Vec(Vec(Vec(Flow(Fragment(Vec(Bits(12 bits), 2))), 2), mr), nr)
    val align = Array.fill(2)(SystolicAlign(Vec(Bits(24 bits), 2), mr))

    for (i <- 0 until nr) {
      for (j <- 0 until mr) {
        for (s <- 0 until 2) {
          matrixOutputs(i)(j)(s).valid := matrix.io.outputs(i)(j).valid
          matrixOutputs(i)(j)(s).last := matrix.io.outputs(i)(j).last
          matrixOutputs(i)(j)(s).fragment := Vec(matrix.io.outputs(i)(j).fragment.slice(s * 2, s * 2 + 2))
        }
      }
    }

    inputsPipe >> s2t.io.inputs
    s2t.io.outputs >> matrix.io.inputs

    val acc = Array.fill(2)(Array.fill(mr)(IntAccChain(nr, 12, compensate = false)))
    for (b <- 0 until 2) {
      for (i <- 0 until mr) {
        for (j <- 0 until nr) {
          acc(b)(i).io.inputs(j) << m2sPipe(matrixOutputs(j)(i)(b))
        }
        m2sPipe(acc(b)(i).io.outputs) >> align(b).io.inputs(i)
      }
    }
  }
  val outputs = Flow(Fragment(Vec(Bits(24 bits), m))) addTag crossClockDomain
  outputs.valid := ddr.align.head.io.outputs.valid
  outputs.last := ddr.align.head.io.outputs.last
  for (i <- 0 until mr) {
    for (b <- 0 until 2) {
      for (s <- 0 until 2) {
        val index = i * 4 + b * 2 + s
        outputs.fragment(index) := ddr.align(b).io.outputs.fragment(i)(s)
      }
    }
  }
  outputs >> io.outputs
}
