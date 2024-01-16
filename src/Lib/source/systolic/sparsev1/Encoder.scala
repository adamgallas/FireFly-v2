package systolic.sparsev1

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Encoder[TD <: Data, TS <: Data](
                                                      denseDataType: HardType[TD],
                                                      sparseDataType: HardType[TS],
                                                      denseFifoDepth: Int,
                                                      factor: Int
                                                    ) extends Component {
  val io = new Bundle {
    val dense = slave(Stream(denseDataType))
    val sparse = slave(Stream(Fragment(Vec(sparseDataType, factor))))
    val outputs = master(Flow(Fragment(utils.SparseData(denseDataType, factor))))
  }
  noIoPrefix()

  val isAllZero = io.sparse.fragment === io.sparse.fragmentType().getZero
  val join = fifos.QueueAsync(StreamJoin(io.dense, io.sparse).throwWhen(isAllZero && !io.sparse.last), 32)

  val dense = join._1
  val sparse = join._2.fragment
  val branch = StreamFork(join.toEvent(), factor).map(_.toEvent())
  val postFilter = (branch, sparse).zipped.map((x, y) => x.throwWhen(y === sparseDataType().getZero).toEvent())
  val arbiter = new StreamArbiter(NoData, factor)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
  val outputsEvent = Event

  (postFilter, arbiter.io.inputs).zipped.foreach(_ >> _)
  outputsEvent << arbiter.io.output
  outputsEvent.freeRun()

  io.outputs.data := dense
  io.outputs.index := arbiter.io.chosen
  io.outputs.indexOH := arbiter.io.chosenOH
  io.outputs.valid := outputsEvent.valid
  io.outputs.last := (join.valid && join._2.last).fall(False)
}
