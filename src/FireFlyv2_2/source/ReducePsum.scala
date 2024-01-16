import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ReducePsum(inputWidth: Int, outputWidth: Int, length: Int) extends Component {
  val io = new Bundle {
    val inputs = slave(Flow(Vec(Vec(SInt(inputWidth bits), 4), length)))
    val outputs = master(Flow(Vec(Vec(SInt(outputWidth bits), 4), length)))
    val bias = slave(Stream(Vec(Bits(12 bits), length)))
    val spikeLength = in UInt (18 bits)
    val mode = in UInt (2 bits)
    val replicate = in UInt (4 bits)
    val last = out Bool()
  }

  val valid = io.inputs.valid
  val validDly = RegNext(valid, init = False)
  val validDly2 = RegNext(validDly, init = False)
  val data = io.inputs.payload

  val dataUpReduce = RegNext(Vec(data.map(d => d(0) + (d(1) << 1).expand)))
  val dataDownReduce = RegNext(Vec(data.map(d => d(2) + (d(3) << 1).expand)))
  val upDownReduce = RegNext(Vec((dataUpReduce, dataDownReduce).zipped.map((u, d) => u + (d << 2).expand)))

  val dataUpReduce2 = RegNext(Vec(data.map(d => d(0) + (d(1) << 2).expand)))
  val dataDownReduce2 = RegNext(Vec(data.map(d => d(2) + (d(3) << 2).expand)))
  val upDownReduce2 = RegNext(Vec((dataUpReduce2, dataDownReduce2).zipped.map((u, d) => u + (d << 4).expand)))

  val fourBitsLogic = new Area {
    val flow = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
    val vec = History(upDownReduce, 4, validDly2)
    val cnt = UInt(2 bits) setAsReg() init 0
    val cntOvf = cnt === 4 - 1
    when(validDly2)(cnt := cnt + 1)
    flow.valid := validDly2 & cntOvf
    flow.payload := Vec(vec.transpose.map(v => Vec(v.reverse.map(_.resize(outputWidth)))))
  }

  val codingLogic = new Area {
    val flow = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
    val cnt = UInt(4 bits) setAsReg() init 0
    val cntOvf = cnt === io.replicate
    val isCntZero = cnt === 0
    when(validDly2) {
      cnt := cnt + 1
      when(cntOvf)(cnt.clearAll())
    }
    val dataLatch = RegNextWhen(upDownReduce2, validDly2 & isCntZero)
    flow.valid := RegNext(validDly2, init = False)
    flow.payload := Vec(dataLatch.map(s => Vec(Array.fill(4)(s.resize(outputWidth)))))

    //    val curr = upDownReduce
    //    val prev = RegNextWhen(curr, validDly2)
    //    val sel = Bool() setAsReg() init false
    //    val cond = validDly2 & sel
    //    val sum = Vec((curr, prev).zipped.map((c, p) => RegNextWhen((c << 4) + p, cond).resize(outputWidth)))
    //    sel.toggleWhen(validDly2)
    //    flow.valid := Mux(io.replicateCoding, Delay(validDly2, 2, init = False), RegNext(cond, init = False))
    //    flow.payload := Vec(sum.map(s => Vec(Array.fill(4)(s.resize(outputWidth)))))
  }

  val twoBitsLogic = new Area {
    val flow = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
    val curr = Vec(dataUpReduce, dataDownReduce)
    val next = RegNextWhen(curr, validDly)
    val sel = Bool() setAsReg() init false
    sel.toggleWhen(validDly)
    (flow.payload, next(0)).zipped.foreach(_(0) := _.resized)
    (flow.payload, next(1)).zipped.foreach(_(1) := _.resized)
    (flow.payload, curr(0)).zipped.foreach(_(2) := _.resized)
    (flow.payload, curr(1)).zipped.foreach(_(3) := _.resized)
    flow.valid := validDly & sel
  }

  val passThroughLogic = new Area {
    val flow = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
    flow.valid := valid
    flow.payload := Vec(data.map(v => Vec(v.map(_.resize(outputWidth)))))
  }

  val beforeBias = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
  val afterBias = Flow(Vec(Vec(SInt(outputWidth bits), 4), length))
  val vec = Vec(
    fourBitsLogic.flow,
    codingLogic.flow,
    twoBitsLogic.flow,
    passThroughLogic.flow
  )
  beforeBias << vec(io.mode)

  val biasFifo = new StreamFifoHighPerf(io.bias.payload, 512)
  biasFifo.io.push << io.bias
  val cnt = Counter(18 bits)
  val cntOvf = cnt === io.spikeLength
  biasFifo.io.pop.ready := beforeBias.valid && cntOvf
  io.last := cntOvf & beforeBias.valid
  when(beforeBias.valid) {
    cnt.increment()
    when(cntOvf) {
      cnt.clear()
    }
  }
  val biasPayload = biasFifo.io.pop.payload
  afterBias.valid := RegNext(beforeBias.valid, init = False)
  for (it <- 0 until 4) {
    for (im <- 0 until length) {
      afterBias.payload(im)(it) := RegNext(beforeBias.payload(im)(it) + biasPayload(im).asSInt)
    }
  }
  io.outputs << afterBias
}

object ReducePsum extends App {
  SpinalVerilog(new ReducePsum(12, 18, 16))
}