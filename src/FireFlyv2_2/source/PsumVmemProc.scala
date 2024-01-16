import fifos.StreamTransposeFifo
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class PsumVmemProc(n: Int, length: Int, nodeType:String) extends Component {
  val io = new Bundle {
    val bias = slave(Stream(Vec(Bits(12 bits), length)))
    val threshold = slave(Stream(Vec(Bits(16 bits),length)))
    val inputs = slave(Flow(Fragment(Vec(Vec(Bits(12 bits), 4), length))))
    val outputs = master(Flow(Fragment(Vec(Bits(4 bits), length))))
    val vmemOutputs = master(Flow(Vec(Vec(SInt(16 bits),4), length)))
    val last = out Bool()
  }

  val cfg = new Bundle {
    val rPass = in UInt (4 bits)
    val tPass = in UInt (4 bits)
    val reduceMode = in UInt (2 bits)
    val spikeLength = in UInt (18 bits)
  }

  def satWithSym(src: SInt, m: Int): SInt = {
    val ret = SInt(src.getWidth - m bit)
    when(src.sign) { //negative process
      when(!src(src.getWidth - 1 downto src.getWidth - m - 1).asBits.andR) {
        ret := -ret.maxValue
      }.otherwise {
        ret := src(src.getWidth - m - 1 downto 0)
      }
    }.otherwise { //positive process
      when(src(src.getWidth - 2 downto src.getWidth - m - 1).asBits.orR) {
        ret := ret.maxValue
      }.otherwise {
        ret := src(src.getWidth - m - 1 downto 0)
      }
    }
    ret
  }

  val transpose = new StreamTransposeFifo(io.inputs.fragment, 64)
  transpose.io.pop.ready.set()
  transpose.io.push.payload := io.inputs.fragment
  transpose.io.push.valid := io.inputs.valid
  transpose.io.firstDim := n - 1
  transpose.io.secondDim := cfg.rPass.resized

  val reduce = new ReducePsum(12, 18, length)
  reduce.io.bias << io.bias
  reduce.io.mode := cfg.reduceMode
  reduce.io.spikeLength := cfg.spikeLength
  reduce.io.replicate := cfg.rPass
  io.last := reduce.io.last
  reduce.io.inputs.valid := transpose.io.pop.valid
  (reduce.io.inputs.payload, transpose.io.pop.payload).zipped.foreach((dst, src) => (dst, src).zipped.foreach(_ := _.asSInt))

  val reduceSat = Flow(Fragment(Vec(Vec(SInt(16 bits), 4), length)))
  reduceSat.valid := RegNext(reduce.io.outputs.valid, init = False)
  (reduceSat.fragment, reduce.io.outputs.payload).zipped.foreach((dst, src) => (dst, src).zipped.foreach((d, s) => d := RegNext(satWithSym(s, 2))))

  val tPassCnt = UInt(4 bits) setAsReg() init 0
  val tPassCntOvf = tPassCnt === cfg.tPass
  reduceSat.last := tPassCntOvf
  when(reduceSat.valid) {
    tPassCnt := tPassCnt + 1
    when(tPassCntOvf) {
      tPassCnt := 0
    }
  }

  val thresholdFifo = new fifos.StreamFifoHighPerf(io.threshold.payload, 512)
  thresholdFifo.io.push << io.threshold
  val cnt = Counter(18 bits)
  val cntOvf = cnt === cfg.spikeLength
  thresholdFifo.io.pop.ready := reduceSat.valid && cntOvf
  when(reduceSat.valid) {
    cnt.increment()
    when(cntOvf) {
      cnt.clear()
    }
  }
  val thresholdPayload = thresholdFifo.io.pop.payload

  val dynamics = if (nodeType == "lif") new LIF4StepsDynamicVec(16, length) else new IF4StepsDynamicVec(16, length)
  (dynamics.io.threshold, thresholdPayload).zipped.foreach(_ := _.asSInt)
  dynamics.io.inputs << reduceSat

  io.outputs.valid := dynamics.io.outSpikes.valid
  io.outputs.last := dynamics.io.outSpikes.last
  (io.outputs.fragment, dynamics.io.outSpikes.fragment).zipped.foreach(_ := _.asBits)

  io.vmemOutputs.valid := RegNext(reduceSat.valid, init = False)
  io.vmemOutputs.payload := RegNext(reduceSat.fragment)
}

object PsumVmemProc extends App {
  SpinalVerilog(new PsumVmemProc(16, 16, "if"))
}