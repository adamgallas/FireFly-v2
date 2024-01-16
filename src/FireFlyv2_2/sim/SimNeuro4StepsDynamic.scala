import spinal.core._
import spinal.lib._
import spinal.core.sim._
import utils.{RandomSInt, RandomUInt}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

object SimNeuro4StepsDynamic {

  val isLif = true

  def node(src: Array[Int], threshold: Int) = {
    val spike = ArrayBuffer[Boolean]()
    val vmemBuf = ArrayBuffer[Int]()
    var vmem = 0
    for (i <- src.indices) {
      vmem =
        if (isLif)
        //          (vmem >> 1) + (src(i) >> 1)
          ((vmem + src(i)) / 2.0).floor.toInt
        else
          vmem + src(i)
      vmemBuf += vmem
      spike += (vmem > threshold)
      if (vmem > threshold) vmem = 0
    }
    (spike.toArray, vmemBuf.toArray)
  }

  val samples = 8196
  val fragment = 1
  val threshold = 0
  val psum = Array.fill(samples)(Array.fill(fragment * 4)(RandomSInt(16)))
  val spike = psum.map(v => node(v, threshold)._1)
  val vmem = psum.map(v => node(v, threshold)._2)
  val minVmem = vmem.flatten.min

  println(minVmem)

  val psumFlow = psum.map(_.grouped(4).toArray)
  val spikeFlow = spike.flatten

  def inDriver(dst: Vec[SInt], src: Array[Int]) = {
    (dst, src).zipped.foreach(_ #= _)
  }

  def outDriver(src: Fragment[Vec[Bool]]) = {
    src.fragment.map(_.toBoolean).toArray
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withFstWave.compile(
      rtl = if (isLif) new LIF4StepsDynamic(16) else new IF4StepsDynamic(16)
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)
      dut.io.threshold #= threshold
      fork {
        dut.io.inputs.valid #= false
        dut.io.inputs.last #= false
        dut.clockDomain.waitSampling(32)
        for (i <- 0 until samples) {
          utils.SimArray2FlowFragment(dut.io.inputs, psumFlow(i), dut.clockDomain, inDriver, alwaysValid = false)
        }
        dut.io.inputs.valid #= false
        dut.io.inputs.last #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        val ret = utils.SimFlow2Array(dut.io.outSpikes, samples * fragment, dut.clockDomain, outDriver).toArray.flatten
        dut.clockDomain.waitSampling(32)
        val goldSpikeNum = spikeFlow.count(_ == true)
        val testSpikeNum = ret.count(_ == true)
        val err = (ret, spikeFlow).zipped.map((a, b) => (a != b).toInt).sum
        println(s"err = $err, ${err * 100.0 / ret.length}%")
        println(s"goldSpikeNum = $goldSpikeNum, ${goldSpikeNum * 100.0 / ret.length}%")
        println(s"testSpikeNum = $testSpikeNum, ${testSpikeNum * 100.0 / ret.length}%")
        (ret, spikeFlow).zipped.foreach((a, b) => assert(a == b))
        simSuccess()
      }
    }
  }
}
