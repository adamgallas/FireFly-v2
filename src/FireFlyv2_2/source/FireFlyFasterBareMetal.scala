import fifos.StreamTransposeFifo
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class FireFlyFasterBareMetal(
                                   ddrClk: ClockDomain,
                                   parallelism: (Int, Int, Int, Int),
                                   im2colDepth: Int,
                                   im2colBufTech: String,
                                   weightDepth: Int,
                                   inputStreamBytes: Int,
                                   paramStreamBytes: Int,
                                   outputStreamBytes: Int,
                                   nodeType:String
                                 ) extends Component {
  val (m, v, n, t) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val auxInputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val params = slave(Stream(Fragment(Bits(paramStreamBytes * 8 bits))))
    val outputs = master(Stream(Bits(outputStreamBytes * 8 bits)))
    val last = out Bool()
  }

  val cfgMisc = new Bundle {
    val isConcat = in Bool()
    val concatLength = in Vec(UInt(8 bits), 2)
    val weightReuse = in UInt (16 bits)
    val weightLength = in UInt (log2Up(weightDepth) bits)
    val transposeFirstDim = in UInt (10 bits)
    val transposeSecondDim = in UInt (10 bits)
  }

  val inputs = Stream(Vec(Bits(t bits), v))
  val auxInputs = Stream(Vec(Bits(t bits), v))
  val concatInputs = Stream(Vec(Bits(t bits), v))
  val shortCutInputs = Stream(Vec(Bits(t bits), m))
  val weights = Stream(Vec(Vec(Bits(8 bits), v), m))
  val biasThresholdAlign = Stream(Vec(Bits(32 bits), m))
  val bias = Stream(Vec(Bits(12 bits), m))
  val threshold = Stream(Vec(Bits(16 bits), m))
  val outputs = Stream(Vec(Bits(t bits), m))

  val branch = utils.StreamDemuxSwitchWhenLast(io.params, 2).map(utils.StreamDropLast(_))
  val toBiasThreshold = branch(0)
  val toWeights = branch(1)

  StreamWidthAdapter(io.inputs, inputs)
  //  StreamWidthAdapter(io.auxInputs, auxInputs)
  StreamWidthAdapter(toWeights, weights)
  StreamWidthAdapter(toBiasThreshold, biasThresholdAlign)
  StreamWidthAdapter(outputs, io.outputs)

  val auxDeMux = StreamDemux(io.auxInputs, cfgMisc.isConcat.asUInt, 2)
  StreamWidthAdapter(auxDeMux(0).s2mPipe().m2sPipe(), shortCutInputs)
  StreamWidthAdapter(auxDeMux(1).s2mPipe().m2sPipe(), concatInputs)

  val forks = StreamFork2(biasThresholdAlign)

  bias.arbitrationFrom(forks._1)
  (bias.payload, forks._1.payload).zipped.foreach(_ := _.takeLow(12))

  threshold.arbitrationFrom(forks._2)
  (threshold.payload, forks._2.payload).zipped.foreach(_ := _.takeHigh(16))

  val interleaver = new ConcatInterleaver(inputs.payload, 8)
  val transpose = new StreamTransposeFifo(inputs.payload, 1024)
  val core = new SpikeWeightCalcInt8AddWrapper(ddrClk, parallelism, im2colDepth, im2colBufTech, weightDepth)
  val psumVmem = new PsumVmemProc(n, m, nodeType)
  val postSpike = new PostSpikeProc(m)

  val psumVmemCfg = psumVmem.cfg.toIo()
  val postSpikeCfg = postSpike.cfg.toIo()
  val coreCfg = core.cfg.toIo()

  interleaver.io.length := cfgMisc.concatLength
  core.cfgWeight.weightReuse := cfgMisc.weightReuse
  core.cfgWeight.weightLength := cfgMisc.weightLength
  core.io.ready := postSpike.io.ready
  io.last := psumVmem.io.last

  inputs >> interleaver.io.inputs(0)
  concatInputs >> interleaver.io.inputs(1)

  transpose.io.push << interleaver.io.outputs
  transpose.io.firstDim := cfgMisc.transposeFirstDim
  transpose.io.secondDim := cfgMisc.transposeSecondDim

  core.io.inputs << transpose.io.pop.s2mPipe().m2sPipe()
  core.io.weights << weights
  core.io.outputs >> psumVmem.io.inputs
  psumVmem.io.bias << bias
  psumVmem.io.threshold << threshold

  postSpike.io.input << psumVmem.io.outputs
  postSpike.io.vmemInput << psumVmem.io.vmemOutputs
  postSpike.io.shortcut << shortCutInputs
  postSpike.io.output >> outputs

  val status = core.status.toIo()
}

object FireFlyFasterBareMetal extends App {
  SpinalVerilog(
    new FireFlyFasterBareMetal(
      ClockDomain.external("ddrClk"),
      (16, 16, 8, 4),
      4096 * 8,
      "ultra",
      1024,
      8,
      16,
      8,
      "if"
    )
  )
}