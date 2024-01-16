package systolic.sparsev2

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SparseGen(parallelism: (Int, Int), depth: Int) extends Component {
  val (m, v) = parallelism
  val io = new Bundle {
    val dense = slave(Flow(Fragment(Vec(Vec(Vec(Bits(8 bits), m), v), 2))))
    val sparse = Vec(slave(Stream(Fragment(Vec(Bool(), v)))), 4)
    val outputs = Vec(master(Flow(Vec(Bits(8 bits), m))), 4)
  }
  noIoPrefix()
  val ram = new Ram4R1W(Vec(Vec(Bits(8 bits), m), v), depth)
  io.dense >> ram.io.write

  val encoder = for (i <- 0 until 4) yield new Area {
    val sparseWithAddr = utils.StreamLinkedCounter(io.sparse(i), log2Up(depth))
    val isAllZero = sparseWithAddr.value === sparseWithAddr.valueType().getZero
    val preFilter = fifos.QueueAsync(sparseWithAddr.throwWhen(isAllZero & !sparseWithAddr.last), 32)

    val sparse = preFilter.value
    val branch = StreamFork(preFilter.toEvent(), v).map(_.toEvent())
    val postFilter = (branch, sparse).zipped.map((x, y) => x.throwWhen(y === False).toEvent())

    val arbiter = new StreamArbiter(NoData, v)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none)
    (postFilter, arbiter.io.inputs).zipped.foreach(_ >> _)

    val selector = Flow(UInt(log2Up(v) bits))
    val address = Flow(UInt(log2Up(depth) bits))
    selector.valid := arbiter.io.output.valid
    selector.payload := arbiter.io.chosen
    address.valid := arbiter.io.output.valid
    address.payload := preFilter.linked
    arbiter.io.output.ready.set()
  }

  (ram.io.readCmd, encoder).zipped.map(_ << _.address)
  val denseData = ram.io.readData.map(_.payload)
  val denseSel = encoder.map(e => Delay(e.selector.payload, 2))
  val data = (denseData, denseSel).zipped.map((d, s) => RegNext(d(s)))

  (io.outputs, data).zipped.foreach(_.payload := _)
  (io.outputs, ram.io.readData).zipped.foreach((d, s) => d.valid := RegNext(s.valid, init = False))
}

object SparseGen extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    targetDirectory = "data//verilog//Lib",
    anonymSignalPrefix = "tmp"
  ).generateVerilog(
    new SparseGen((32, 8), 1024)
  )
}
