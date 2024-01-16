import spinal.core._
import spinal.lib._
import systolic.MsVsNsT.Int6MsVsNsTAcc
import systolic._

import scala.language.postfixOps

case class SystolicInt8AddWithOvfWrapper(ddrClk: ClockDomain, parallelism: (Int, Int, Int, Int)) extends Component {
  val (m, v, n, t) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Bits(t bits), v), n), Vec(Vec(Bits(8 bits), v), m)))))
    val outputs = master(Flow(Fragment(Vec(Vec(Bits(12 bits), m), t))))
  }

  def m2sPipe[T <: Data](in: Flow[Fragment[T]]) = {
    val ret = Flow(Fragment(in.fragmentType)) addTag crossClockDomain
    ret.valid := RegNext(in.valid, init = False) addTag crossClockDomain
    ret.last := RegNext(in.last, init = False) addTag crossClockDomain
    ret.fragment := RegNext(in.fragment) addTag crossClockDomain
    ret
  }

  val dataType = HardType(
    utils.Linked(
      Vec(Vec(Vec(Bits(t bits), 2), vr), nr),
      Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), vr), mr)
    )
  )
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
    val matrix = Int8VecAddAccWithOvf((mr, vr, nr, t))
    val outputs = Flow(Fragment(Vec(Vec(Bits(14 bits), m), t))) addTag crossClockDomain

    inputsPipe >> s2t.io.inputs
    s2t.io.outputs >> matrix.io.inputs

    outputs.valid := matrix.io.outputs.valid
    outputs.last := matrix.io.outputs.last
    for (i <- 0 until t) {
      for (j <- 0 until m) {
        outputs.fragment(i)(j) := matrix.io.outputs.fragment(j / 4)(i)(j % 4)
      }
    }
  }
  val slowDown = new SlowDownBuffer(ddrClk, Vec(Vec(Bits(14 bits), m), t), nr, nr)
  ddr.outputs >> slowDown.io.inputs

  val beforeSat = slowDown.io.outputs.fragment
  val afterSat = Vec(Vec(Bits(12 bits), m), t)
  for (it <- 0 until t) {
    for (im <- 0 until m) {
      val posOvf = beforeSat(it)(im).msb
      val negOvf = beforeSat(it)(im).dropHigh(1).msb
      val data = beforeSat(it)(im).dropHigh(2).asSInt
      afterSat(it)(im) := data.asBits
      when(posOvf)(afterSat(it)(im) := 0x7FF)
      when(negOvf)(afterSat(it)(im) := 0x800)
    }
  }

  io.outputs.valid := RegNext(slowDown.io.outputs.valid, init = False)
  io.outputs.last := RegNext(slowDown.io.outputs.last, init = False)
  io.outputs.fragment := RegNext(afterSat)
}

object SystolicInt8AddWithOvfWrapper {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new SystolicInt8AddWithOvfWrapper(ClockDomain.external("ddrClk"), (16, 16, 8, 4)))
  }
}