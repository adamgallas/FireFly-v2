import spinal.core._
import spinal.lib._
import systolic._

import scala.language.postfixOps

case class Int8VecAddAcc(parallelism: (Int, Int, Int, Int)) extends Component {
  val (m, v, n, t) = parallelism
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(Vec(Bits(t bits), 2), v), n), Vec(Vec(Vec(Vec(Bits(8 bits), 4), 2), v), m)))))
    val outputs = master(Flow(Fragment(Vec(Vec(Vec(Bits(12 bits), 4), t), m))))
  }
  noIoPrefix()

  val init = SystolicInit(Vec(Bits(t bits), 2), Vec(Vec(Bits(8 bits), 4), 2), (m, v, n))
  val chain = Array.fill(n)(Array.fill(m)(Int8VecAdd(t, v)))
  val gather = Array.fill(m)(new LadderFlowGather(Vec(Vec(Bits(12 bits), 4), t), n))
  val align = SystolicAlign(Vec(Vec(Bits(12 bits), 4), t), m)

  io.inputs >> init.io.inputs

  for (in <- 0 until n) {
    for (im <- 0 until m) {
      chain(in)(im).io.inputs << init.io.outputs(in)(im)
      gather(im).io.inputs(in) << chain(in)(im).io.outputs
    }
  }

  (gather, align.io.inputs).zipped.foreach(_.io.outputs >> _)
  align.io.outputs >> io.outputs
}

object Int8VecAddAcc extends App {
  SpinalVerilog(new Int8VecAddAcc((4, 4, 8, 4)))
}