import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class NeuroDynamic(width: Int) extends Component {
  val io = new Bundle {
    val threshold = in SInt (width bits)
    val inputs = slave(Flow(Fragment(Vec(SInt(width bits), 4))))
    val outSpikes = master(Flow(Fragment(Vec(Bool(), 4))))
  }
}