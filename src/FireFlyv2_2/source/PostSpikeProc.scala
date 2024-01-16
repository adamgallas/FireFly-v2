import fifos.StreamTransposeFifo
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class PostSpikeProc(length: Int) extends Component {
  val io = new Bundle {
    val input = slave(Flow(Fragment(Vec(Bits(4 bits), length))))
    val vmemInput = slave(Flow(Vec(Vec(SInt(16 bits), 4), length)))

    val shortcut = slave(Stream(Vec(Bits(4 bits), length)))
    val output = master(Stream(Vec(Bits(4 bits), length)))
    val ready = out Bool()
  }
  val cfg = new Bundle {
    val tPass = in UInt (4 bits)
    val enableMaxpool = in Bool()
    val enableAvgPool = in Bool()
    val enableSew = in Bool()
    val columns = in UInt (10 bits)
    val packLength = in UInt (10 bits)
    val validLength = in UInt (10 bits)
    val muxSel = in UInt (2 bits)

    val enableVmemOut = in Bool() default False
    val enableSpikeAcc = in Bool() default False
    val spikeAccTwoBit = in Bool() default False
    val spikeAccLength = in UInt (8 bits) default 0
  }

  def maxpoolOp(a: Vec[Bits], b: Vec[Bits]) = {
    Vec((a, b).zipped.map((av, bv) => Vec((av.asBools, bv.asBools).zipped.map(_ | _)).asBits))
  }

  def avgpoolOp1(a: Vec[Bits], b: Vec[Bits]) = {
    Vec((a, b).zipped.map((av, bv) => Vec((av.asBools, bv.asBools).zipped.map(_.asUInt.expand + _.asUInt.expand))))
  }

  def avgpoolOp2(a: Vec[Vec[UInt]], b: Vec[Vec[UInt]]) = {
    Vec((a, b).zipped.map((av, bv) => Vec((av, bv).zipped.map((a, b) => (a.expand + b.expand).sat(1)))))
  }

  val transpose = new StreamTransposeFifo(io.input.fragment, 32)
  val maxpool = new SimplePool2x2(io.input.fragment, io.input.fragment, io.input.fragment, 1024, maxpoolOp, maxpoolOp)
  val avgpool = new SimplePool2x2(io.input.fragment, Vec(Vec(UInt(2 bits), 4), length), Vec(Vec(UInt(2 bits), 4), length), 1024, avgpoolOp1, avgpoolOp2)

  transpose.io.push.valid := io.input.valid & ~cfg.enableVmemOut
  transpose.io.push.payload := io.input.fragment
  transpose.io.firstDim := cfg.tPass.resized
  transpose.io.secondDim := U(1).resized
  transpose.io.pop.ready.set()

  maxpool.io.input.valid := transpose.io.pop.valid
  maxpool.io.input.fragment := transpose.io.pop.payload

  avgpool.io.input.valid := transpose.io.pop.valid
  avgpool.io.input.fragment := transpose.io.pop.payload

  val colCnt = UInt(10 bits) setAsReg() init 0
  val colCntOvf = colCnt === cfg.columns

  maxpool.io.input.last := colCntOvf
  avgpool.io.input.last := colCntOvf

  when(transpose.io.pop.valid) {
    colCnt := colCnt + 1
    when(colCntOvf) {
      colCnt := 0
    }
  }

  val beforePool = Flow(Fragment(Vec(Bits(4 bits), length)))
  val afterMaxpool = Flow(Fragment(Vec(Bits(4 bits), length)))
  val afterAvgPool = Flow(Fragment(Vec(Vec(UInt(2 bits), 4), length)))
  val flow2Stream = new SpikeFlow2Stream(4, length)

  beforePool.valid := io.input.valid
  beforePool.last.clear()
  beforePool.fragment := io.input.fragment

  afterMaxpool.valid := maxpool.io.output.valid
  afterMaxpool.last.clear()
  afterMaxpool.fragment := maxpool.io.output.fragment

  afterAvgPool.valid := RegNext(avgpool.io.output.valid, False)
  afterAvgPool.last.clear()
  afterAvgPool.fragment := RegNextWhen(avgpool.io.output.fragment, avgpool.io.output.valid)

  val afterHalfAvg = Flow(Fragment(Vec(Bits(4 bits), length)))
  val sel = Bool() setAsReg() init False
  sel.toggleWhen(afterHalfAvg.valid)

  val firstHalf = Vec(afterAvgPool.fragment.map(v => Vec(v.take(2)).asBits))
  val secondHalf = Vec(afterAvgPool.fragment.map(v => Vec(v.drop(2)).asBits))
  afterHalfAvg.fragment := Mux(sel = sel, whenTrue = secondHalf, whenFalse = firstHalf)
  afterHalfAvg.valid := afterAvgPool.valid || RegNext(afterAvgPool.valid, False)
  afterHalfAvg.last.clear()

  flow2Stream.io.packLength := cfg.packLength
  flow2Stream.io.validLength := cfg.validLength
  flow2Stream.io.input << Vec(beforePool, afterMaxpool, afterHalfAvg, beforePool)((cfg.enableAvgPool ## cfg.enableMaxpool).asUInt)
  io.ready := flow2Stream.io.ready

  val streamOut = Stream(Vec(Bits(4 bits), length))
  val streamOuts = StreamDemux(streamOut, cfg.enableSew.asUInt, 2)
  streamOut.arbitrationFrom(flow2Stream.io.output)
  streamOut.payload := flow2Stream.io.output.fragment

  val sewConnect = new SewConnect(length)
  val sewOut = Stream(Vec(Bits(4 bits), length))
  sewConnect.io.muxSel := cfg.muxSel
  sewConnect.io.input << streamOuts(1)
  sewConnect.io.shortCut << io.shortcut
  sewOut << StreamMux(cfg.enableSew.asUInt, Vec(streamOuts(0), sewConnect.io.output))

  val spikeAcc = new SpikeAcc(length)
  val sewOuts = StreamDemux(sewOut.s2mPipe().m2sPipe(), cfg.enableSpikeAcc.asUInt, 2)
  val spikeOut = Stream(Vec(Bits(4 bits), length))
  spikeAcc.io.twoBitAdd := cfg.spikeAccTwoBit
  spikeAcc.io.total := cfg.spikeAccLength
  spikeAcc.io.input << sewOuts(1)
  spikeOut << StreamMux(cfg.enableSpikeAcc.asUInt, Vec(sewOuts(0), spikeAcc.io.output))

  val vmemFlow2Stream = new VmemFlow2Stream(length)
  val vmemOut = Stream(Vec(Bits(4 bits), length))
  vmemFlow2Stream.io.packLength := cfg.packLength
  vmemFlow2Stream.io.validLength := cfg.validLength
  vmemFlow2Stream.io.input.valid := io.vmemInput.valid & cfg.enableVmemOut
  vmemFlow2Stream.io.input.payload := io.vmemInput.payload


  io.output << StreamMux(cfg.enableVmemOut.asUInt, Vec(spikeOut, vmemFlow2Stream.io.output))
}

object PostSpikeProc extends App {
  SpinalVerilog(new PostSpikeProc(16))
}