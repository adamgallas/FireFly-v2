package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SystolicInit[TW <: Data, TI <: Data](
                                                 inputType: HardType[TI],
                                                 weightType: HardType[TW],
                                                 parallelism: (Int, Int, Int)
                                               ) extends Component {
  val (m, v, n) = parallelism
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(Vec(inputType, v), n), Vec(Vec(weightType, v), m)))))
    val outputs = Vec(Vec(master(Flow(Fragment(utils.Linked(Vec(inputType, v), Vec(weightType, v))))), m), n)
  }
  noIoPrefix()

  val weightEdgeInit = Vec(io.inputs.linked.map(s => Vec(s.zipWithIndex.map(e => Delay(e._1, e._2)))))
  val inputEdgeInit = Vec(io.inputs.value.map(s => Vec(s.zipWithIndex.map(e => Delay(e._1, e._2)))))
  val validInit = Delay(io.inputs.valid, v - 1, init = False)
  val lastInit = Delay(io.inputs.last, v - 1, init = False)

  val weightEdge = Vec(weightEdgeInit.zipWithIndex.map(e => Delay(e._1, e._2)))
  val inputEdge = Vec(inputEdgeInit.zipWithIndex.map(e => Delay(e._1, e._2)))

  val weightHistory = History(weightEdge, n)
  val inputHistory = History(inputEdge, m)
  val validHistory = History(History(validInit, m, init = False), n, init = Vec(Array.fill(m)(False)))
  val lastHistory = History(History(lastInit, m, init = False), n, init = Vec(Array.fill(m)(False)))

  for (in <- 0 until n) {
    for (im <- 0 until m) {
      io.outputs(in)(im).valid := validHistory(in)(im)
      io.outputs(in)(im).last := lastHistory(in)(im)
      io.outputs(in)(im).linked := weightHistory(in)(im)
      io.outputs(in)(im).value := inputHistory(im)(in)
    }
  }
}
