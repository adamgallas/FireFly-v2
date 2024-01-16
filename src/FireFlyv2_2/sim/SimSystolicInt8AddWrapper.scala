import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

object SimSystolicInt8AddWrapper {

  def calc_spike(src: Array[Int], thresh: Int) = {
    val spike = ArrayBuffer[Boolean]()
    var vmem = 0
    for (i <- src.indices) {
      val update = vmem + src(i)
      val fire = update > thresh
      spike.append(fire)
      vmem = if (fire) 0 else update
    }
    spike.toArray
  }

  val parallelism = (8, 8, 4, 4)
  val (m, v, n, t) = parallelism
  val length = 4
  val pass = 8

  val linkData = Array.fill(pass)(Array.fill(length)((
    Array.fill(n)(Array.fill(v)(Random.nextInt(1 << t))),
    Array.fill(m)(Array.fill(v)(Random.nextInt(64) - 32))
  )))
  val thresh = Random.nextInt(64)

  val res = Array.ofDim[Int](pass, m, n, t)

  for(p<-0 until pass) {
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        for (r <- 0 until t) {
          res(p)(i)(j)(r) = 0
          for (s <- 0 until v) {
            for (l <- 0 until length) {
              val mult = ((linkData(p)(l)._1(j)(s) >> r) & 1) * linkData(p)(l)._2(i)(s)
              res(p)(i)(j)(r) += mult
            }
          }
        }
      }
    }
  }

  val res_spike = res.map(_.map(_.map(calc_spike(_, thresh))))

  def inputDriver(drain: utils.Linked[Vec[Vec[Bits]], Vec[Vec[Bits]]], source: (Array[Array[Int]], Array[Array[Int]])) = {
    (drain.value, source._1).zipped.foreach((d, s) => (d, s).zipped.foreach(_ #= _ & ((1 << t) - 1)))
    (drain.linked, source._2).zipped.foreach((d, s) => (d, s).zipped.foreach(_ #= _ & ((1 << 8) - 1)))
  }

  def outputDriver(source: Fragment[Vec[Vec[Bits]]]) = {
    source.fragment.map(v => v.map(s => (s.toInt << 20) >> 20).toArray).toArray
  }


  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.withWave.allOptimisation.addRtl("data/sim/DSP48E2.v").compile(
      rtl = SystolicInt8AddWrapper(ClockDomain.external("ddrClk"), parallelism)
    )

    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(4)
      dut.ddrClk.forkStimulus(2)

      fork {
        dut.io.inputs.valid #= false
        dut.io.inputs.last #= false
        dut.clockDomain.waitSampling(64)
        for (p <- 0 until pass) {
          utils.SimArray2FlowFragment(dut.io.inputs, linkData(p), dut.clockDomain, inputDriver, alwaysValid = false)
          dut.io.inputs.valid #= false
          dut.io.inputs.last #= false
          //          dut.clockDomain.waitSampling(1)
        }
        dut.clockDomain.waitSampling(2)
      }

      fork {
        for (p <- 0 until pass) {
          val ret = utils.SimFlow2Array(dut.io.outputs, n, dut.clockDomain, outputDriver).reverse.toArray
          for (r <- 0 until t) {
            for (i <- 0 until m) {
              for (j <- 0 until n) {
                val test = ret(j)(r)(i)
                val ref = res(p)(i)(j)(r)
                if (test != ref) {
                  println(s"Error at ($r, $i, $j): $test != $ref")
                }
              }
            }
          }
          println("----pass----", p)
        }
        simSuccess()
      }
    }
  }
}
