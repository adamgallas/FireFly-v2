package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.tools.BigIntToListBoolean

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object ConstRangeVecGen {
  def apply(data: UInt, range: Int, width: Int) = {
    val len = log2Up(range)
    val powOf2 = Range(0, len).map(i => (data ## B(0, i bits)).asUInt)
    Vec(for (i <- 0 to range) yield {
      val boolList = BigIntToListBoolean(i, len bits)
      val buf = ArrayBuffer[UInt]()
      for (b <- 0 until len) {
        if (boolList(b))
          buf.append(powOf2(b))
      }
      (if (buf.nonEmpty) buf.toArray.reduce(_ + _)
      else U(0)).resize(width)
    })
  }
}

//case class ConstRangeVecGenTest() extends Component {
//  val a = in UInt (8 bits)
//  val b = out Vec(UInt(8 bits), 8)
//  b := ConstRangeVecGen(a, 7, 8)
//}
//
//object SimConstRangeVecGenTest {
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(ConstRangeVecGenTest()).doSim { dut =>
//      dut.a #= 0
//      for (i <- 0 until 16) {
//        dut.a #= i
//        sleep(10)
//        println(i, "----------------")
//        for (j <- 0 to 7) {
//          println(dut.b(j).toBigInt)
//        }
//      }
//    }
//  }
//}