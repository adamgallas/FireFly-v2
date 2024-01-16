package utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


object AxiLite2BigWidthData {
  def apply(ctrl: AxiLite4SlaveFactory, bundle: Bundle, baseAddr: BigInt) = {
    val bitWidth = bundle.getBitsWidth
    val roundedBitWidth = (bitWidth + 31) / 32 * 32
    val regCount = roundedBitWidth / 32

    val axiLite2bits = new Area {
      val regs = Vec(Reg(UInt(32 bits)) init (0), regCount)
      for (i <- 0 until regCount) {
        ctrl.write(regs(i), baseAddr + i * 4, 0)
      }
      val flatBits = regs.asBits
      bundle.assignFromBits(flatBits.takeLow(bitWidth))
    }
  }
}
