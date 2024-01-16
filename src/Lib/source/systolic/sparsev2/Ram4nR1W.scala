package systolic.sparsev2

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Ram4nR1W[T <: Data](dataType: HardType[T], depth: Int, ports: Int) extends Component {
  require(ports % 4 == 0)
  val numOfBank = ports / 4
  val bankSelWidth = log2Up(numOfBank)
  val addressWidth = log2Up(depth)

  val io = new Bundle {
    val write = slave(Flow(Fragment(Vec(dataType, ports / 2))))
    val readCmd = Vec(slave(Stream(UInt(addressWidth bits))), ports)
    val readData = Vec(master(Flow(dataType)), ports)
  }
  noIoPrefix()

  val bank = Array.fill(numOfBank)(new Ram4R1W(dataType, depth / numOfBank))
  bank.foreach(_.io.write.valid := io.write.valid)
  bank.foreach(_.io.write.last := io.write.last)

  val writeData = io.write.fragment.grouped(ports / 4).toIndexedSeq
  (bank, writeData(0)).zipped.foreach(_.io.write.fragment(0) := _)
  (bank, writeData(1)).zipped.foreach(_.io.write.fragment(1) := _)

  val grpOfCmdSrc = io.readCmd.grouped(4).toIndexedSeq.transpose
  val grpOfCmdDst = bank.map(_.io.readCmd.toIndexedSeq).toIndexedSeq.transpose
  val grpOfDataDst = io.readData.grouped(4).toIndexedSeq.transpose
  val grpOfDataSrc = bank.map(_.io.readData.toIndexedSeq).toIndexedSeq.transpose

  val grpOfArbiter = for (i <- 0 until 4) yield new Area {
    val (cmdSrc, cmdDst) = (grpOfCmdSrc(i), grpOfCmdDst(i))
    val (dataSrc, dataDst) = (grpOfDataSrc(i), grpOfDataDst(i))

    val sel = cmdSrc.map(_.payload.take(bankSelWidth).asUInt)
    val branch = (cmdSrc, sel).zipped.map((c, s) => StreamDemux(c.toEvent(), s, numOfBank))
    val arbiter = Array.fill(numOfBank)(new StreamArbiter(NoData, numOfBank)(StreamArbiter.Arbitration.roundRobin, StreamArbiter.Lock.none))
    for (i <- 0 until numOfBank) {
      for (j <- 0 until numOfBank) {
        arbiter(i).io.inputs(j) << branch(j)(i)
      }
    }
    val outputsEvent = arbiter.map(_.io.output)
    outputsEvent.foreach(_.freeRun())

    val chosen = arbiter.map(c => RegNext(c.io.chosen))
    val addr = Vec(cmdSrc.map(a => RegNext(a.payload.drop(bankSelWidth).asUInt)))
    val cmd = chosen.map(c => RegNext(addr(c)))
    val data = Vec(dataSrc.map(_.payload))
    (cmdDst, outputsEvent).zipped.foreach((d, s) => d.valid := Delay(s.valid, 2, init = False))
    (cmdDst, cmd).zipped.foreach(_.payload := _)

    val chosenOH = Vec(arbiter.map(c => RegNext(c.io.chosenOH))).as(Vec(Vec(Bool(), numOfBank), numOfBank)).transpose
    val dec = Delay(Vec(chosenOH.map(s => OHToUInt(Vec(s).asBits))), 3)
    val resRouted = dec.map(d => RegNext(data(d)))

    (dataDst, resRouted).zipped.foreach(_.payload := _)
    (dataDst, cmdSrc).zipped.foreach((d, s) => d.valid := Delay(s.fire, 5, init = False))
  }
}

object Ram4nR1W extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    targetDirectory = "data//verilog//Lib",
    anonymSignalPrefix = "tmp"
  ).generateVerilog(
    new Ram4nR1W(Bits(64 bits), 4096, 16)
  )
}
