import spinal.core.sim._
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object SimSpikeAcc {

  val alwaysValid = false
  val alwaysReady = false

  val group = 64
  val length = 64

  val twoBit = true

  val spike = Array.fill(group)(Array.fill(length)(Random.nextInt(if (twoBit) 4 else 2)))
  val spikeSum = spike.map(_.sum)
  val low = spikeSum.map(_ & 0xf)
  val high = spikeSum.map(_ >> 4)
  val spikeSeq = spike.flatten.grouped(if (twoBit) 2 else 4).toArray
  val sumSeq = (low, high).zipped.flatMap((l, h) => Array(l, h))

  def inDriver(dst: Vec[Bits], src: Array[Int]) = {
    if (twoBit)
      dst(0) #= src(0) + (src(1) << 2)
    else
      dst(0) #= src(0) + (src(1) << 1) + (src(2) << 2) + (src(3) << 3)
  }

  def outDriver(src: Vec[Bits]) = {
    src(0).toInt
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withFstWave.compile(
      rtl = new SpikeAcc(1)
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)
      dut.io.twoBitAdd #= twoBit
      dut.io.total #= length / (if(twoBit) 2 else 4) - 1

      fork {
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(32)
        utils.SimArray2Stream(dut.io.input, spikeSeq, dut.clockDomain, inDriver, alwaysValid)
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        val ret = utils.SimStream2Array(dut.io.output, sumSeq.length, dut.clockDomain, outDriver, alwaysReady).toArray
        dut.clockDomain.waitSampling(32)
        for (i <- sumSeq.indices) {
          assert(ret(i) == sumSeq(i))
        }
        simSuccess()
      }
    }
  }

}
