import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

object SimFragmentConcat {

  val pass = 8
  val x1InputData = Array.fill(pass)(Array.fill(16)(Random.nextInt(32)))
  val x2InputData = Array.fill(pass)(Array.fill(32)(Random.nextInt(32)).grouped(2).map(_.toArray).toArray)

  val isX1Input = false
  val isX2Output = false

  def x1Driver(dst: UInt, src: Int) = {
    dst #= src
  }

  def x2Driver(dst: Vec[UInt], src: Array[Int]) = {
    (dst, src).zipped.foreach(_ #= _)
  }

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.withWave.compile(
      rtl = new FragmentConcat(UInt(16 bits))
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 4)

      dut.io.x1Input.valid #= false
      dut.io.x2Input.valid #= false
      dut.io.x1Input.last #= false
      dut.io.x2Input.last #= false
      dut.io.isX1Input #= isX1Input
      dut.io.isX2Output #= isX2Output

      dut.clockDomain.waitSampling(32)
      for (p <- 0 until pass) {
        if (isX1Input)
          utils.SimArray2FlowFragment(dut.io.x1Input, x1InputData(p), dut.clockDomain, x1Driver, alwaysValid = false)
        else
          utils.SimArray2FlowFragment(dut.io.x2Input, x2InputData(p), dut.clockDomain, x2Driver, alwaysValid = false)
      }
      dut.clockDomain.waitSampling(128)
      simSuccess()
    }
  }

}
