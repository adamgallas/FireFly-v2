import spinal.core._
import spinal.lib._

import scala.language.{dynamics, postfixOps}

class NeuroDynamicVec(width: Int, length: Int) extends Component {
  val io = new Bundle {
    val threshold = in Vec(SInt (width bits), length)
    val inputs = slave(Flow(Fragment(Vec(Vec(SInt(width bits), 4), length))))
    val outSpikes = master(Flow(Fragment(Vec(Vec(Bool(), 4), length))))
  }
}