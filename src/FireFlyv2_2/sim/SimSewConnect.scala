import spinal.core.sim._
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object SimSewConnect {

  val samples = 16
  val length = 8

  def booleanArray2Bits(src: Array[Boolean]) = {
    src.reverse.foldLeft(0)((i, b) => (i << 1) + (if (b) 1 else 0))
  }

  def inDriver(dst: Vec[Bits], src: Array[Array[Boolean]]) = {
    (dst, src).zipped.foreach((d, s) => d #= booleanArray2Bits(s))
  }

  def shortCutDriver(dst: Vec[Bits], src: Array[Int]) = {
    (dst, src).zipped.foreach(_ #= _)
  }

  def outDriver(src: Vec[Bits]) = {
    src.map(_.toInt).toArray
  }

  def clamp(data: Int, min: Int, max: Int) = {
    if (data < min) min else if (data > max) max else data
  }

  val caseNumber = 2
  val alwaysValid = true
  val alwaysReady = true

  // case 0
  val case0Input = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextBoolean())))
  val case0Shortcut = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextBoolean())))
  val case0ResOut = (case0Input, case0Shortcut).zipped.map((iv, sv) => (iv, sv).zipped.map((i, s) => (i, s).zipped.map(!_ & _)))
  val case0GoldenData = Array.ofDim[Int](samples, length)
  for (s <- 0 until samples) {
    for (l <- 0 until length) {
      case0GoldenData(s)(l) = booleanArray2Bits(case0ResOut(s)(l))
    }
  }

  // case 1
  val case1Input = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextBoolean())))
  val case1Shortcut = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextBoolean())))
  val case1ResOut = (case1Input, case1Shortcut).zipped.map((iv, sv) => (iv, sv).zipped.map((i, s) => (i, s).zipped.map(_.toInt + _.toInt)))
  val case1GoldenData = Array.ofDim[Int](samples * 2, length)
  for (s <- 0 until samples) {
    for (l <- 0 until length) {
      case1GoldenData(s * 2)(l) = case1ResOut(s)(l)(0) + (case1ResOut(s)(l)(1) << 2)
      case1GoldenData(s * 2 + 1)(l) = case1ResOut(s)(l)(2) + (case1ResOut(s)(l)(3) << 2)
    }
  }

  // case 2
  val case2Input = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextBoolean())))
  val case2Shortcut = Array.fill(samples)(Array.fill(length)(Array.fill(4)(Random.nextInt(4))))
  val case2ShortCutReshape = Array.ofDim[Int](samples * 2, length)
  for (s <- 0 until samples) {
    for (l <- 0 until length) {
      case2ShortCutReshape(s * 2)(l) = case2Shortcut(s)(l)(0) + (case2Shortcut(s)(l)(1) << 2)
      case2ShortCutReshape(s * 2 + 1)(l) = case2Shortcut(s)(l)(2) + (case2Shortcut(s)(l)(3) << 2)
    }
  }
  val case2ResOut = (case2Input, case2Shortcut).zipped.map((iv, sv) => (iv, sv).zipped.map((i, s) => (i, s).zipped.map(_.toInt + _.toInt)))
  val case2GoldenData_2bits = Array.ofDim[Int](samples * 2, length)
  for (s <- 0 until samples) {
    for (l <- 0 until length) {
      case2GoldenData_2bits(s * 2)(l) = clamp(case2ResOut(s)(l)(0), 0, 3) + (clamp(case2ResOut(s)(l)(1), 0, 3) << 2)
      case2GoldenData_2bits(s * 2 + 1)(l) = clamp(case2ResOut(s)(l)(2), 0, 3) + (clamp(case2ResOut(s)(l)(3), 0, 3) << 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withFstWave.compile(
      rtl = new SewConnect(length)
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)

      dut.io.muxSel #= caseNumber

      fork {
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(32)

        caseNumber match {
          case 0 => utils.SimArray2Stream(dut.io.input, case0Input, dut.clockDomain, inDriver, alwaysValid)
          case 1 => utils.SimArray2Stream(dut.io.input, case1Input, dut.clockDomain, inDriver, alwaysValid)
          case 2 => utils.SimArray2Stream(dut.io.input, case2Input, dut.clockDomain, inDriver, alwaysValid)
        }

        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        dut.io.shortCut.valid #= false
        dut.clockDomain.waitSampling(32)

        caseNumber match {
          case 0 => utils.SimArray2Stream(dut.io.shortCut, case0Shortcut, dut.clockDomain, inDriver, alwaysValid)
          case 1 => utils.SimArray2Stream(dut.io.shortCut, case1Shortcut, dut.clockDomain, inDriver, alwaysValid)
          case 2 => utils.SimArray2Stream(dut.io.shortCut, case2ShortCutReshape, dut.clockDomain, shortCutDriver, alwaysValid)
        }

        dut.io.shortCut.valid #= false
        dut.clockDomain.waitSampling(32)
      }

      fork {
        dut.io.output.ready #= false
        dut.clockDomain.waitSampling(32)
        val outputSamples = caseNumber match {
          case 0 => samples
          case 1 => samples * 2
          case 2 => samples * 2
        }

        val ret = utils.SimStream2Array(dut.io.output, outputSamples, dut.clockDomain, outDriver, alwaysReady).toArray
        val goldData = caseNumber match {
          case 0 => case0GoldenData
          case 1 => case1GoldenData
          case 2 => case2GoldenData_2bits
        }

        dut.io.output.ready #= false
        dut.clockDomain.waitSampling(32)
        (ret, goldData).zipped.foreach((rv, gv) => (rv, gv).zipped.foreach((r, v) => assert(r == v)))
        simSuccess()
      }

    }
  }
}