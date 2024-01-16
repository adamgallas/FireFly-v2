import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

object SimIm2colWithT {
  val timeStep = 1
  val channels = 512
  val imgSize = (4, 4)
  val padding = (1, 1)
  val kernel = (3, 3)
  val dilation = (1, 1)
  val stride = (1, 1)
  val (pp, cp) = (8, 16)
  val pass = 32

  val gen = new Im2colWithTSimGen(
    timeStep = timeStep,
    channels = channels,
    imgSize = imgSize,
    padding = padding,
    kernel = kernel,
    dilation = dilation,
    stride = stride,
    parallelism = (pp, cp),
    reuse = pass
  )

  gen.genImg()
  gen.genPadImg()
  gen.genTransImg()
  val trans = gen.genTransImgStream()
  println("buffer depth", gen.bufferDepth)

  def driver(drain: Vec[UInt], source: Array[Int]): Unit = {
    (drain, source).zipped.foreach(_ #= _)
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.withFstWave.allOptimisation.compile(
      rtl = new Im2colWithT(UInt(8 bits), cp, pp, 4096 * gen.pp)
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 10)
//      SimTimeout(1000000)

      val start = simTime()
      gen.setConfig(dut)

      fork {
//        for (iter <- 0 until pass) {
          dut.io.push.valid #= false
//          dut.clockDomain.waitSampling(64)
          utils.SimArray2Stream(dut.io.push, gen.genInImgStream(), dut.clockDomain, driver, alwaysValid = true)
          dut.io.push.valid #= false
//          dut.clockDomain.waitSampling(64)
//        }
      }

      fork {
        var index = 0
        for (iter <- 0 until pass) {
          index = 0
          dut.io.pop.ready #= false
//          dut.clockDomain.waitSampling(32)
          while (index < trans.length) {
            dut.io.pop.ready #= true
            dut.clockDomain.waitSampling()
            if (dut.io.pop.ready.toBoolean && dut.io.pop.valid.toBoolean) {
              val test = dut.io.pop.payload
              val gold = trans(index)
              for (p <- 0 until pp) {
                for (c <- 0 until cp) {
                  if (test(p)(c).toInt != gold(p)(c))
                    println(s"Error: test = ${test(p)(c).toInt}, gold = ${gold(p)(c)}, index = $index")
                  //assert(test(p)(c).toInt == gold(p)(c))
                }
              }
              index = index + 1
            }
          }
          dut.io.pop.ready #= false
          dut.clockDomain.waitSampling(32)
          println("task finish", iter)
        }

        val end = simTime()
        println(s"total time: ${(end - start) / 1000} us")
        simSuccess()
      }
    }
  }
}
