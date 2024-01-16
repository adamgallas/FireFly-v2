package systolic.sparsev2

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Encoder[TD <: Data, TS <: Data](
                                            denseDataType: HardType[TD],
                                            sparseDataType: HardType[TS],
                                            factor: Int
                                          ) extends Component {
  val io = new Bundle {
    val denseAddr = master(Stream(UInt(8 bits)))
    val denseData = slave(Stream(Vec(denseDataType, factor)))
    val sparse = slave(Stream(Fragment(Vec(sparseDataType, factor))))
    val output = master(Flow(denseDataType))
  }
  noIoPrefix()

  val encode = new Area {
    val sel = Stream(UInt(log2Up(factor) bits))
    val halt = Stream(UInt(log2Up(factor) + 1 bits))

    val sparseWithAddr = utils.StreamLinkedCounter(io.sparse, 8)
    val isAllZero = sparseWithAddr.value === sparseWithAddr.valueType().getZero
    val preFilter = fifos.QueueAsync(sparseWithAddr.throwWhen(isAllZero & !sparseWithAddr.last), 32)
    val (toRam, toArbiter, toHalt) = StreamFork3(preFilter)

    val sparse = toArbiter.value
    val branch = StreamFork(toArbiter.toEvent(), factor).map(_.toEvent())
    val postFilter = (branch, sparse).zipped.map((x, y) => x.throwWhen(y === sparseDataType().getZero).toEvent())

    val arbiter = new StreamArbiter(NoData, factor)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
    (postFilter, arbiter.io.inputs).zipped.foreach(_ >> _)

    val selector = Stream(UInt(log2Up(factor) bits))
    val cnt = CountOne(Vec(toHalt.value.map(_ =/= sparseDataType().getZero)).asBits)

    selector.arbitrationFrom(arbiter.io.output)
    selector.payload := arbiter.io.chosen
    fifos.Queue(toHalt.translateWith(cnt), 32) >> halt
    fifos.Queue(toRam.translateWith(toRam.linked), 32) >> io.denseAddr
    fifos.Queue(selector, 32) >> sel
  }

  val sync = new Area {
    val hold = utils.StreamHold(io.denseData, encode.halt)
    val join = StreamJoin(encode.sel, hold)
    join.freeRun()
    io.output.valid := RegNext(join.valid, init = False)
    io.output.payload := RegNext(join._2(join._1))
  }
}
