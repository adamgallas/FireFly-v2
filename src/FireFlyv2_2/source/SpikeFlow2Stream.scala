import spinal.core._
import spinal.lib._
import fifos.StreamFifoHighPerf
import scala.language.postfixOps

class SpikeFlow2Stream(timeStep: Int, length: Int) extends Component {
  val io = new Bundle {
    val packLength = in UInt (10 bits)
    val validLength = in UInt (10 bits)
    val ready = out Bool()

    val input = slave(Flow(Fragment(Vec(Bits(timeStep bits), length))))
    val output = master(Stream(Fragment(Vec(Bits(timeStep bits), length))))
  }

  val spikeCnt = Counter(10 bits)
  val spikeCntOvf = spikeCnt === io.packLength
  val spikeThrowCond = spikeCnt > io.validLength
  when(io.input.valid) {
    spikeCnt.increment()
    when(spikeCntOvf) {
      spikeCnt.clear()
    }
  }

  val spikeFifoLv1 = StreamFifo(io.output.payload, 1024, forFMax = true)
  spikeFifoLv1.io.push.valid := io.input.valid && !spikeThrowCond
  spikeFifoLv1.io.push.fragment := io.input.fragment
  spikeFifoLv1.io.push.last := io.input.last
  io.ready := spikeFifoLv1.io.pop.ready

  val spikeFifoLv2 = StreamFifo(spikeFifoLv1.io.pop.payload, 1024, forFMax = true)
  spikeFifoLv2.io.push << spikeFifoLv1.io.pop
  io.output << spikeFifoLv2.io.pop
}

object SpikeFlow2Stream extends App {
  SpinalVerilog(new SpikeFlow2Stream(4, 16))
}