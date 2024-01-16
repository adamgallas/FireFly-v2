import spinal.core._
import spinal.lib._
import fifos.StreamFifoHighPerf
import scala.language.postfixOps

class VmemFlow2Stream(length: Int) extends Component {
  val io = new Bundle {
    val packLength = in UInt (10 bits)
    val validLength = in UInt (10 bits)
    val ready = out Bool()

    val input = slave(Flow(Vec(Vec(SInt(16 bits), 4), length)))
    val output = master(Stream(Vec(Bits(4 bits),length)))
  }

  val vmemCnt = Counter(10 bits)
  val vmemCntOvf = vmemCnt === io.packLength
  val vmemThrowCond = vmemCnt > io.validLength
  when(io.input.valid) {
    vmemCnt.increment()
    when(vmemCntOvf) {
      vmemCnt.clear()
    }
  }

  val spikeFifoLv1 = StreamFifo(io.input.payload, 512, forFMax = true)
  val spikeFifoLv2 = StreamFifo(io.output.payload, 512, forFMax = true)

  spikeFifoLv1.io.push.valid := io.input.valid && !vmemThrowCond
  spikeFifoLv1.io.push.payload := io.input.payload
  io.ready := spikeFifoLv2.io.push.ready

  StreamWidthAdapter(spikeFifoLv1.io.pop.m2sPipe(), spikeFifoLv2.io.push)
  io.output << spikeFifoLv2.io.pop
}

object VmemFlow2Stream extends App {
  SpinalVerilog(new VmemFlow2Stream(16))
}