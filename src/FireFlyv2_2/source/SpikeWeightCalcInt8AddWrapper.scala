import fifos.{StreamCycleFifo, StreamFifoHighPerf}
import im2col.Im2colWrapper
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SpikeWeightCalcInt8AddWrapper(
                                          ddrClk: ClockDomain,
                                          parallelism: (Int, Int, Int, Int),
                                          im2colDepth: Int,
                                          im2colBufTech: String,
                                          weightDepth: Int
                                        ) extends Component {
  val (m, v, n, t) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Stream(Vec(Bits(t bits), v)))
    val weights = slave(Stream(Vec(Vec(Bits(8 bits), v), m)))
    //    val bias = slave(Stream(Vec(Bits(12 bits), m)))
    val outputs = master(Flow(Fragment(Vec(Vec(Bits(12 bits), t), m))))
    //    val length = in UInt (18 bits)
    val ready = in Bool()
    //    val last = out Bool()
  }

  val im2col = new Im2colWithTWrapper(Bits(t bits), v, n, im2colDepth, im2colBufTech)
  val weightCycle = new StreamCycleFifo(Vec(Vec(Bits(8 bits), v), m), weightDepth, 16)
  val systolic = new SystolicInt8AddWithOvfWrapper(ddrClk, parallelism)
  im2col.io.push << io.inputs
  weightCycle.io.push << io.weights

  val cfg = im2col.config.toIo()

  val cfgWeight = new Bundle {
    val weightReuse = in UInt (16 bits)
    val weightLength = in UInt (log2Up(weightDepth) bits)
  }

  weightCycle.io.reuse := cfgWeight.weightReuse
  weightCycle.io.length := cfgWeight.weightLength

  val joinValid = im2col.io.pop.valid && weightCycle.io.pop.valid && io.ready
  im2col.io.pop.ready := weightCycle.io.pop.valid && io.ready
  weightCycle.io.pop.ready := im2col.io.pop.valid && io.ready

  val lastGenCnt = UInt(log2Up(weightDepth) bits) setAsReg() init 0
  val lastReach = lastGenCnt === cfgWeight.weightLength
  when(joinValid) {
    lastGenCnt := lastGenCnt + 1
    when(lastReach) {
      lastGenCnt.clearAll()
    }
  }

  systolic.io.inputs.valid := joinValid
  systolic.io.inputs.last := joinValid && lastReach
  systolic.io.inputs.value := Vec(im2col.io.pop.payload.reverse)
  systolic.io.inputs.linked := weightCycle.io.pop.payload

  //  val biasFifo = new StreamFifoHighPerf(io.bias.payload, 512)
  //  biasFifo.io.push << io.bias
  //  val outputs = Flow(Fragment(Vec(Vec(Bits(12 bits), t), m)))
  //  val biasPayload = biasFifo.io.pop.payload
  //  for (it <- 0 until t) {
  //    for (im <- 0 until m) {
  //      outputs.fragment(im)(it) := RegNext(systolic.io.outputs.fragment(it)(im).asSInt + biasPayload(im).asSInt).asBits
  //    }
  //  }
  //  val cnt = Counter(18 bits)
  //  val cntOvf = cnt === io.length
  //  biasFifo.io.pop.ready := systolic.io.outputs.valid && cntOvf
  //  io.last := systolic.io.outputs.valid && cntOvf
  //  when(systolic.io.outputs.valid) {
  //    cnt.increment()
  //    when(cntOvf) {
  //      cnt.clear()
  //    }
  //  }
  //
  //  outputs.valid := RegNext(systolic.io.outputs.valid, init = False)
  //  outputs.last := RegNext(systolic.io.outputs.last, init = False)
  //  io.outputs << outputs

  io.outputs.valid := systolic.io.outputs.valid
  io.outputs.last := systolic.io.outputs.last
  io.outputs.fragment := Vec(systolic.io.outputs.fragment.transpose.map(v => Vec(v)))

  val status = new Bundle {
    val ready = out Bool()
    val im2colPopValid = out Bool()
    val weightPopValid = out Bool()
    val im2colPopReady = out Bool()
    val weightPopReady = out Bool()
    val joinValid = out Bool()
    val inCnt = out UInt (13 bits)
    val outCnt = out UInt (13 bits)
  }

  val inCnt = UInt(13 bits) setAsReg() init 0
  val outCnt = UInt(13 bits) setAsReg() init 0
  when(systolic.io.inputs.valid & systolic.io.inputs.last) {
    inCnt := inCnt + 1
  }
  when(systolic.io.outputs.valid) {
    outCnt := outCnt + 1
  }

  status.ready := RegNext(io.ready)
  status.im2colPopValid := RegNext(im2col.io.pop.valid)
  status.weightPopValid := RegNext(weightCycle.io.pop.valid)
  status.im2colPopReady := RegNext(im2col.io.pop.ready)
  status.weightPopReady := RegNext(weightCycle.io.pop.ready)
  status.joinValid := RegNext(joinValid)
  status.inCnt := RegNext(inCnt)
  status.outCnt := RegNext(outCnt)
}

object SpikeWeightCalcInt8AddWrapper extends App {
  SpinalVerilog(
    new SpikeWeightCalcInt8AddWrapper(
      ClockDomain.external("ddrClk"),
      (16, 16, 8, 4),
      8192 * 8,
      "ultra",
      1024
    )
  )
}
