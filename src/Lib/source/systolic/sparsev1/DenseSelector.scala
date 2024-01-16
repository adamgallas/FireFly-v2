package systolic.sparsev1

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DenseSelector[T <: Data](dataType: HardType[T], factor: Int) extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.SparseData(Vec(dataType, factor), factor))))
    val outputs = master(Flow(Fragment(dataType)))
  }
  noIoPrefix()
  val chosen = RegNext(io.inputs.index)
  val data = RegNext(io.inputs.data)
  val dataChosen = RegNext(data(chosen))
  io.outputs.fragment := dataChosen
  io.outputs.valid := Delay(io.inputs.valid, 2, init = False)
  io.outputs.last := Delay(io.inputs.last, 2, init = False)
}
