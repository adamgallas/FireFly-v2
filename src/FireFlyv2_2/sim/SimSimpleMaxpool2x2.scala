import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.tools.BigIntToListBoolean

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

object SimSimpleMaxpool2x2 {

  val row = 16
  val col = 16
  val length = 8
  val beforePool = Array.fill(row)(Array.fill(col)(Array.fill(length)(Array.fill(4)(Random.nextBoolean()))))
  val afterPool = Array.ofDim[Boolean](row / 2, col / 2, length, 4)

  for (r <- 0 until row / 2) {
    for (c <- 0 until col / 2) {
      for (l <- 0 until length) {
        for (i <- 0 until 4) {
          afterPool(r)(c)(l)(i) =
            beforePool(r * 2)(c * 2)(l)(i) ||
              beforePool(r * 2)(c * 2 + 1)(l)(i) ||
              beforePool(r * 2 + 1)(c * 2)(l)(i) ||
              beforePool(r * 2 + 1)(c * 2 + 1)(l)(i)
        }
      }
    }
  }

  def maxpoolOp(a: Vec[Bits], b: Vec[Bits]) = {
    Vec((a, b).zipped.map((av, bv) => Vec((av.asBools, bv.asBools).zipped.map(_ || _)).asBits))
  }

  def booleanArray2Bits(src: Array[Boolean]) = {
    src.reverse.foldLeft(0)((i, b) => (i << 1) + (if (b) 1 else 0))
  }

  def inDriver(dst: Vec[Bits], src: Array[Array[Boolean]]) = {
    (dst, src).zipped.foreach((d, s) => d #= booleanArray2Bits(s))
  }

  def outDriver(src: Fragment[Vec[Bits]]) = {
    src.fragment.map(s => BigIntToListBoolean(s.toBigInt, s.getWidth bits).toArray).toArray
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withWave.compile(
      rtl = new SimplePool2x2(Vec(Bits(4 bits), length), Vec(Bits(4 bits), length), Vec(Bits(4 bits), length), 128, maxpoolOp, maxpoolOp)
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)

      fork {
        dut.io.input.valid #= false
        dut.io.input.last #= false
        dut.clockDomain.waitSampling(32)
        for (r <- 0 until row) {
          utils.SimArray2FlowFragment(dut.io.input, beforePool(r), dut.clockDomain, inDriver, alwaysValid = false)
        }
        dut.io.input.valid #= false
        dut.io.input.last #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        val ret = utils.SimFlow2Array(dut.io.output, row * col / 4, dut.clockDomain, outDriver).toArray
        val test = ret.grouped(col / 2).toArray
        for (r <- 0 until row / 2) {
          for (c <- 0 until col / 2) {
            for (l <- 0 until length) {
              for (t <- 0 until 4) {
                assert(test(r)(c)(l)(t) == afterPool(r)(c)(l)(t))
              }
            }
          }
        }
        dut.clockDomain.waitSampling(32)
        simSuccess()
      }
    }
  }
}
