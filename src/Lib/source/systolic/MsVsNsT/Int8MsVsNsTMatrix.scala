package systolic.MsVsNsT

import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

case class Int8MsVsNsTMatrix(parallelism: (Int, Int, Int, Int)) extends Component {
  val (m, v, n, t) = parallelism
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Vec(Bits(t bits), 2), v), n), Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), v), m)))))
    val outputs = Vec(Vec(master(Flow(Fragment(Vec(Vec(Bits(12 bits), 4), t)))), m), n)
  }
  noIoPrefix()

  val init = SystolicInit(Vec(Bits(t bits), 2), Vec(Vec(Bits(8 bits), 4), 2), (m, v, n))
  val chain = Array.fill(n)(Array.fill(m)(Int8VsVsTChain(t, v)))
  io.inputs >> init.io.inputs

  for (in <- 0 until n) {
    for (im <- 0 until m) {
      chain(in)(im).io.inputs << init.io.outputs(in)(im)
      io.outputs(in)(im) << chain(in)(im).io.outputs
    }
  }
}
