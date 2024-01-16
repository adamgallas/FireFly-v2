package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SystolicAlign[T <: Data](dataType: HardType[T], m: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Flow(Fragment(dataType))), m)
    val outputs = master(Flow(Fragment(Vec(dataType, m))))
  }
  io.outputs.valid := io.inputs.last.valid
  io.outputs.last := io.inputs.last.last
  val payload = io.inputs.zipWithIndex.map(f => Delay(f._1.fragment, m - f._2 - 1))
  (io.outputs.fragment, payload).zipped.foreach(_ := _)
}
