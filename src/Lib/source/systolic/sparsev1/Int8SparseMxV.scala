package systolic.sparsev1

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Int8SparseMxV(parallelism: (Int, Int)) extends Component {
  val (m, v) = parallelism
  val vr = v / 2

  val io = new Bundle {
    val dense = slave(Stream(Vec(Vec(Bits(8 bits), m), v)))
    val sparse = slave(Stream(Fragment(Vec(Bits(1 bits), v))))
    val outputs = master(Flow(Fragment(Vec(Bits(24 bits), 2))))
  }

  val dense = utils.StreamForkSplitVec(io.dense, pipe = false)
  val sparse = utils.StreamForkSplitVec.fragment(io.sparse, pipe = false)
  val encoder = Array.fill(2)(Encoder(Vec(Vec(Bits(8 bits), m), vr), Bits(1 bits), 8, vr))
  val selector = Array.fill(2)(DenseSelector(Vec(Bits(8 bits), m), vr))
  val acc = DenseAccChain(m, Bits(8 bits).getBitsWidth)

  (dense, encoder).zipped.foreach(_ >> _.io.dense)
  (sparse, encoder).zipped.foreach(_ >> _.io.sparse)
  (encoder, selector).zipped.foreach(_.io.outputs >> _.io.inputs)
  (selector, acc.io.inputs).zipped.foreach(_.io.outputs >> _)
  io.outputs << acc.io.outputs
}
