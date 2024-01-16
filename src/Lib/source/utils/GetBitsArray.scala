package utils

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object GetBitsArray {
  def apply[T <: Data](input: T): Array[Bits] = {
    val bits = ArrayBuffer[Bits]()
    input match {
      case m: MultiData =>
        for (elem <- m.elements)
          bits ++= GetBitsArray(elem._2)
      case s => bits ++= s.as(Vec(Bits(1 bits), s.getBitsWidth))
    }
    bits.toArray
  }
}
