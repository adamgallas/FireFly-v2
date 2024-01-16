package systolic.MsVsN

import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

case class Int8MsVsNMatrix(parallelism: (Int, Int, Int)) extends Component {
  val (m, v, n) = parallelism
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Vec(Bits(1 bits), 2), v), n), Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), v), m)))))
    val outputs = Vec(Vec(master(Flow(Fragment(Vec(Bits(12 bits), 4)))), m), n)
  }
  noIoPrefix()

  val init = SystolicInit(Vec(Bits(1 bits), 2), Vec(Vec(Bits(8 bits), 4), 2), parallelism)
  val chain = Array.fill(n)(Array.fill(m)(Int8VsVChain(v)))
  io.inputs >> init.io.inputs
  for (in <- 0 until n) {
    for (im <- 0 until m) {
      chain(in)(im).io.inputs << init.io.outputs(in)(im)
      io.outputs(in)(im) << chain(in)(im).io.outputs
    }
  }
}
