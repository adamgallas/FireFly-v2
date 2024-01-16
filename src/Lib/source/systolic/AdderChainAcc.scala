package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class AdderChainAcc[TI <: Data, TW <: Data, TO <: Data](
                                                      valueDataType: HardType[TI],
                                                      linkedDataType: HardType[TW],
                                                      outDataType: HardType[TO],
                                                      length: Int,
                                                      needInit: Boolean = false
                                                    ) extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Fragment(utils.Linked(Vec(valueDataType, length), Vec(linkedDataType, length)))))
    val outputs = master(Flow(outDataType))
  }

  val inputs = Flow(Fragment(utils.Linked(Vec(valueDataType, length), Vec(linkedDataType, length))))
  if (needInit) {
    inputs.valid := Delay(io.inputs.valid, length - 1, init = False)
    inputs.last := Delay(io.inputs.last, length - 1, init = False)
    inputs.value := Vec(io.inputs.value.zipWithIndex.map(e => Delay(e._1, e._2)))
    inputs.linked := Vec(io.inputs.linked.zipWithIndex.map(e => Delay(e._1, e._2)))
  }
  else {
    inputs << io.inputs
  }
}
