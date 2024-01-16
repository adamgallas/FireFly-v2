import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.core.sim.{forkJoin, _}
import spinal.lib._
import ai.djl.ndarray.NDManager
import ai.djl.ndarray.types.{DataType, Shape}
import utils.RandomUInt

import scala.language.postfixOps
import scala.util.Random


object SimFireFlyFasterMultiTask {
  val alwaysValid = true
  val alwaysReady = true
  val manager = NDManager.newBaseManager
  val parallelism = (16, 16, 8, 4)
  val busChannel = (8, 32, 8)
  val im2colDepth = 4096 * 8

  val frequency = 250
  val period = 1000.0 / frequency
  val scale = period / 4.0

  val modelCfg = CfgParser("data/yaml/snn5_3x3.yaml", parallelism, busChannel, im2colDepth)
  val totalTasks = modelCfg.cfg_list.length
  val taskCfgList = modelCfg.cfg_list

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.addRtl("data/sim/DSP48E2.v").compile(
      rtl = FireFlyFasterCfgWrapper(
        ClockDomain.external("ddrClk"),
        modelCfg.core.parallelism,
        modelCfg.core.im2colDepth,
        "ultra", 1024,
        modelCfg.core.busChannel._1,
        modelCfg.core.busChannel._2,
        modelCfg.core.busChannel._3,
        modelCfg.core.nodeType
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(4)
      dut.ddrClk.forkStimulus(2)
      //      SimTimeout(1000000)

      val start = simTime()

      dut.io.inputs.valid #= false
      dut.io.params.valid #= false
      dut.io.params.last #= false
      dut.io.outputs.ready #= false
      dut.io.auxInputs.valid #= false
      dut.configList.config0 #= 0
      dut.configList.config1 #= 0
      dut.configList.config2 #= 0
      dut.configList.config3 #= 0
      dut.configList.config4 #= 0
      dut.configList.config5 #= 0
      dut.configList.im2colConfig0 #= 0
      dut.configList.im2colConfig1 #= 0
      dut.configList.im2colConfig2 #= 0
      dut.configList.im2colConfig3 #= 0
      dut.configList.im2colConfig4 #= 0
      dut.configList.im2colConfig5 #= 0
      dut.configList.im2colConfig6 #= 0
      dut.configList.im2colConfig7 #= 0
      dut.configList.im2colConfig8 #= 0

      dut.clockDomain.waitSampling(32)
      dut.ddrClk.assertReset()
      dut.clockDomain.assertReset()
      sleep(16)
      dut.ddrClk.deassertReset()
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(32)

      for (taskIdx <- 0 until totalTasks) {
        println(s"------------------Task $taskIdx begin------------------")

        val taskStart = simTime()

        //        val taskCfg = new TaskCfg(
        //          inputBits = 1,
        //          shortcutBits = 0,
        //          timeStep = 4,
        //          poolingType = "none",
        //          resConnect = "none",
        //          enableSpikeAcc = false,
        //          enableVmem = false,
        //          convCfg = new ConvCfg(
        //            channels = (32, 64),
        //            imgSize = (64, 64),
        //            padding = (1, 1),
        //            kernel = (3, 3),
        //            dilation = (1, 1),
        //            stride = (1, 1)
        //          )
        //        )

        val ideal = (64 / dut.m) *
          (64 * 64 / dut.n) *
          9 *
          (32 / dut.v) *
          (4 * 1 / dut.t)

        println(f"ideal time: ${ideal / 1000.0 * scale}%.2f us")

        val taskCfg = taskCfgList(taskIdx)
        val convCfgList = new CfgList4Conv(modelCfg.core, taskCfg)
        val dataGen = new DataGen4Conv(manager, modelCfg.core, taskCfg)
        val cfgWord = SimUtils.gen_config_word(convCfgList.cfgList, dut)

        dut.configList.config0 #= cfgWord.head
        dut.configList.config1 #= cfgWord(1)
        dut.configList.config2 #= cfgWord(2)
        dut.configList.config3 #= cfgWord(3)
        dut.configList.config4 #= cfgWord(4)
        dut.configList.config5 #= cfgWord(5)
        dut.configList.im2colConfig0 #= cfgWord(6)
        dut.configList.im2colConfig1 #= cfgWord(7)
        dut.configList.im2colConfig2 #= cfgWord(8)
        dut.configList.im2colConfig3 #= cfgWord(9)
        dut.configList.im2colConfig4 #= cfgWord(10)
        dut.configList.im2colConfig5 #= cfgWord(11)
        dut.configList.im2colConfig6 #= cfgWord(12)
        dut.configList.im2colConfig7 #= cfgWord(13)
        dut.configList.im2colConfig8 #= cfgWord(14)

        taskCfg.print()
        println("begin forks")
        val inputFork = fork {
          println("input fork activated")
          dut.io.inputs.valid #= false
          dut.clockDomain.waitSampling(32)

          println(s"inputElems: ${convCfgList.im2colCfg.inElems}, bufDepth: ${modelCfg.core.im2colDepth}, reuse: ${convCfgList.im2colCfg.popReuse}")

          if (convCfgList.im2colCfg.isReuse) {
            dut.io.inputs.valid #= false
            utils.SimArray2Stream(dut.io.inputs, dataGen.xStream, dut.clockDomain, SimUtils.InputDriver(taskCfg.in_spike_bits), alwaysValid = alwaysValid)
            dut.io.inputs.valid #= false
            dut.clockDomain.waitSampling(32)
            println("input pass finished, reusing")
          }
          else {
            for (p <- 0 until dataGen.inputPass) {
              dut.io.inputs.valid #= false
              utils.SimArray2Stream(dut.io.inputs, dataGen.xStream, dut.clockDomain, SimUtils.InputDriver(taskCfg.in_spike_bits), alwaysValid = alwaysValid)
              dut.io.inputs.valid #= false
              dut.clockDomain.waitSampling(32)
              println(s"inputPass: $p")
            }
            dut.io.inputs.valid #= false
          }
        }

        val paramFork = fork {
          println("param fork activated")
          dut.io.params.valid #= false
          dut.clockDomain.waitSampling(32)
          utils.SimArray2StreamFragment(dut.io.params, dataGen.btStream, dut.clockDomain, SimUtils.BiasThresholdDriver, alwaysValid = alwaysValid)
          dut.io.params.valid #= false
          dut.clockDomain.waitSampling(32)
          utils.SimArray2StreamFragment(dut.io.params, dataGen.wStream, dut.clockDomain, SimUtils.WeightDriver, alwaysValid = alwaysValid)
          dut.io.params.valid #= false
          println("param finish")
        }

        val auxInputFork = fork {
          println("aux input fork activated")
          dut.io.auxInputs.valid #= false
          dut.clockDomain.waitSampling(32)
          if (taskCfg.res_connect != "none") {
            utils.SimArray2Stream(dut.io.auxInputs, dataGen.shortCutStream, dut.clockDomain, SimUtils.InputDriver(taskCfg.shortcut_spike_bits), alwaysValid = alwaysValid)
          }
          println("shortcut finish")
          dut.io.auxInputs.valid #= false
        }

        val outputFork = fork {
          println("output fork activated")
          if (taskCfg.spike_acc) {
            val ret = utils.SimStream2Array(dut.io.outputs, dataGen.rSumStream.length, dut.clockDomain, SimUtils.sumOutputDriver(busChannel._3), alwaysReady = alwaysReady).toArray
            dut.clockDomain.waitSampling(128)
            val test = ret.flatten
            val gold = dataGen.rSumStream.flatten
            println("acc transfer finished")
            for (i <- test.indices) {
              if (test(i) != gold(i))
                println(s"err at $i, test: ${test(i)}, gold: ${gold(i)}")
            }
            val err = test.zip(gold).map { case (t, g) => if (t != g) 1 else 0 }.sum
            println(s"err: $err, percent: ${err.toDouble / test.length}")
            //            (test, gold).zipped.foreach((t, g) => assert(t == g))
          }
          else if (taskCfg.vmem_out) {
            val ret = utils.SimStream2Array(dut.io.outputs, dataGen.yStream.length, dut.clockDomain, SimUtils.vmemOutDriver(busChannel._3), alwaysReady = alwaysReady).toArray
            dut.clockDomain.waitSampling(128)
            val test = ret.flatten
            val gold = dataGen.yStream.flatten
            println("vmem transfer finished")
            for (i <- test.indices) {
              if (test(i) != gold(i))
                println(s"err at $i, test: ${test(i)}, gold: ${gold(i)}")
            }
            val err = test.zip(gold).map { case (t, g) => if (t != g) 1 else 0 }.sum
            println(s"err: $err, percent: ${err.toDouble / test.length}")
            //            (test, gold).zipped.foreach((t, g) => assert(t == g))
          }
          else {
            val ret = utils.SimStream2Array(dut.io.outputs, dataGen.rStream.length, dut.clockDomain, SimUtils.OutputDriver(busChannel._3, convCfgList.out_spike_bits), alwaysReady = alwaysReady).toArray
            dut.clockDomain.waitSampling(128)
            val test = ret.flatten
            val gold = dataGen.rStream.flatten
            println("spike transfer finished")
            //            for (i <- test.indices) {
            //              if (test(i) != gold(i))
            //                println(s"err at $i, test: ${test(i)}, gold: ${gold(i)}")
            //            }
            val err = test.zip(gold).map { case (t, g) => if (t != g) 1 else 0 }.sum
            println(s"err: $err, percent: ${err.toDouble / test.length}")
            //            (test, gold).zipped.foreach((t, g) => assert(t == g))
          }
        }

        inputFork.join()
        paramFork.join()
        auxInputFork.join()
        outputFork.join()
        println("forks joined")

        val taskEnd = simTime()

        println(f"taskTime: ${(taskEnd - taskStart) / 1000.0 * scale}%.2f us")
        println(f"elapsedTime: ${(taskEnd - start) / 1000.0 * scale}%.2f us")

        println(s"------------------Task $taskIdx done------------------")
      }

      val end = simTime()
      println(f"simTime: ${(end - start) / 1000.0 * scale}%.2f us")

      simSuccess()
    }
  }
}
