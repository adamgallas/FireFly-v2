import im2col.Im2colSimGen
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.tools.BigIntToListBoolean
import utils.RandomSInt

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

import ai.djl.ndarray.NDManager

object SimSpikeWeightCalcInt8AddWrapper {
  val manager = NDManager.newBaseManager
  val parallelism = (16, 16, 8, 4)
  val (m, v, n, t) = parallelism
  val gen = new SpikeWeightCalcWithTDataGen(
    manager = manager,
    parallelism = parallelism,
    timeStep = 4,
    channels = (16, 256),
    imgSize = (32, 32),
    padding = (0, 0),
    kernel = (1, 1),
    dilation = (1, 1),
    stride = (2, 2)
  )

  def inputDriver(drain: Vec[Bits], source: Array[Int]) = {
    (drain, source).zipped.foreach(_ #= _)
  }

  def weightDriver(drain: Vec[Vec[Bits]], source: Array[Array[Int]]) = {
    (drain, source).zipped.foreach((d, s) => {
      (d, s).zipped.foreach(_ #= _ & ((1 << 8) - 1))
    })
  }

  def biasDriver(drain: Vec[Bits], source: Array[Int]) = {
    (drain, source).zipped.foreach((d, s) => d #= s & 0xfff)
  }

  def outputDriver(source: Fragment[Vec[Vec[Bits]]]) = {
    source.fragment.map(v => (v.map(s => (s.toInt << 20) >> 20)).toArray).toArray
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.addRtl("data/sim/DSP48E2.v").compile(
      rtl = SpikeWeightCalcInt8AddWrapper(
        ClockDomain.external("ddrClk"),
        parallelism,
        8192 * 8,
        "ultra",
        1024
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)
      dut.ddrClk.forkStimulus(period = 2)

      dut.io.ready #= true

      dut.cfgWeight.weightReuse #= gen.config.weightReuse
      dut.cfgWeight.weightLength #= gen.config.weightLength
      val inputPass = gen.config.inputPass
      val outputLength = gen.config.outputLength

      dut.cfg.pushChannelsTimeStep #= gen.config.pushChannelsTimeStep
      dut.cfg.pushHeight #= gen.config.pushHeight
      dut.cfg.pushWidth #= gen.config.pushWidth
      dut.cfg.pushHeightReserve #= gen.config.pushHeightReserve

      dut.cfg.popChannels #= gen.config.popChannels
      dut.cfg.timeStep #= gen.config.popTimeSteps
      dut.cfg.popHeight #= gen.config.popHeight
      dut.cfg.popWidth #= gen.config.popWidth
      dut.cfg.popHeightInc #= gen.config.popHeightInc
      dut.cfg.popKernelWidth #= gen.config.popKernelWidth
      dut.cfg.popKernelHeight #= gen.config.popKernelHeight
      dut.cfg.popKernelWidthAddrInc #= gen.config.popKernelWidthAddrInc
      dut.cfg.popKernelHeightAddrInc #= gen.config.popKernelHeightAddrInc
      dut.cfg.popTimeStepInc #= gen.config.popTimeStepAddrInc
      dut.cfg.popWidthAddrInc #= gen.config.popWidthAddrInc
      dut.cfg.popHeightAddrInc #= gen.config.popHeightAddrInc
      dut.cfg.popCoalesceBound #= gen.config.popCoalesceBound
      dut.cfg.popOffset #= gen.config.popOffset

      dut.cfg.pad4ConvShape_0 #= gen.config.pad4ConvShape_0
      dut.cfg.pad4ConvShape_1 #= gen.config.pad4ConvShape_1
      dut.cfg.pad4ConvShape_2 #= gen.config.pad4ConvShape_2
      dut.cfg.pad4ConvHead_1 #= gen.config.pad4ConvHead_1
      dut.cfg.pad4ConvHead_2 #= gen.config.pad4ConvHead_2
      dut.cfg.pad4ConvTail_1 #= gen.config.pad4ConvTail_1
      dut.cfg.pad4ConvTail_2 #= gen.config.pad4ConvTail_2
      dut.cfg.pad4CoalesceShape #= gen.config.pad4CoalesceShape
      dut.cfg.pad4CoalesceTail #= gen.config.pad4CoalesceTail

      fork {
        dut.io.inputs.valid #= false
        dut.clockDomain.waitSampling(32)
        for (p <- 0 until inputPass) {
          dut.io.inputs.valid #= false
          utils.SimArray2Stream(dut.io.inputs, gen.xStream, dut.clockDomain, inputDriver, alwaysValid = true)
          dut.io.inputs.valid #= false
          println(s"inputPass: $p")
//          dut.clockDomain.waitSampling(1024)
        }
      }

      fork {
        //        dut.io.bias.valid #= false
        dut.io.weights.valid #= false

        //        dut.clockDomain.waitSampling(32)
        //        utils.SimArray2Stream(dut.io.bias, gen.bStream, dut.clockDomain, biasDriver, alwaysValid = true)
        //        dut.io.bias.valid #= false
        dut.clockDomain.waitSampling(32)

        utils.SimArray2Stream(dut.io.weights, gen.wStream, dut.clockDomain, weightDriver, alwaysValid = true)
        dut.io.weights.valid #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        println(gen.gen.y.getShape, gen.config.outImgSize, outputLength)
        val testData = utils.SimFlow2Array(dut.io.outputs, outputLength, dut.clockDomain, outputDriver).toArray.flatten.flatten
        val goldData = gen.yStream.flatten.flatten
        println("transfer finished")
        dut.clockDomain.waitSampling(32)

        for ((t, g) <- testData.zip(goldData)) {
          if (t * g<0) {
            println(s"t: $t, g: $g")
          }
        }

        (testData, goldData).zipped.foreach((t, g) => assert(t == g))
        simSuccess()
      }
    }
  }
}