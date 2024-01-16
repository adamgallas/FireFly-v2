package utils

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.language.postfixOps
import scala.util.Random

class HistoryGroup[T <: Data](dataType: HardType[T], group: Int, factor: Int) extends Component {
  val width = dataType().getBitsWidth
  require(width % group == 0)
  val io = new Bundle {
    val input = in(dataType)
    val enable = in Vec(Bool(), group)
    val outputs = out(Vec(dataType, factor))
  }

  val split = io.input.asBits.subdivideIn(group slices)
  val outputs = Vec(Bits(width bits), factor)
  val historys = (split, io.enable).zipped.map((s, e) => History(s, factor + 1, when = e).drop(1).reverse)
  for (f <- 0 until factor) {
    for (g <- 0 until group) {
      for (w <- 0 until width / group) {
        outputs(f)(g * width / group + w) := historys(g)(f)(w)
      }
    }
    io.outputs(f).assignFromBits(outputs(f))
  }
}

//object SimHistoryGroup {
//  val factor = 8
//
//  def main(args: Array[String]): Unit = {
//    val compiled = SimConfig.withWave.allOptimisation.compile(
//      rtl = new HistoryGroup(Bits(8 bits), 4, factor)
//    )
//    compiled.doSimUntilVoid { dut =>
//      dut.clockDomain.forkStimulus(10)
//      dut.io.enable.foreach(_ #= false)
//      dut.clockDomain.waitSampling(32)
//      for (pass <- 0 until 4) {
//        var index = 0
//        while (index < 8) {
//          val enable = Random.nextBoolean()
//          dut.io.enable.foreach(_ #= enable)
//          dut.io.input #= index
//          dut.clockDomain.waitSampling()
//          if(enable){
//            index = index + 1
//          }
//        }
//        dut.io.enable.foreach(_ #= false)
//        dut.clockDomain.waitSampling(32)
//      }
//      simSuccess()
//    }
//  }
//}