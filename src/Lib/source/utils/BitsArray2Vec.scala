package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object BitsArray2Vec {
  def apply[T <: Data](dataType: HardType[T], bits: Array[Bits], n1: Int) = {
    require(bits.length == dataType.getBitsWidth * n1)
    val ret = Vec(dataType, n1)
    val split = bits.grouped(dataType.getBitsWidth).toArray.map(Vec(_).as(dataType))
    var index = 0
    for (iter1 <- 0 until n1) {
      ret(iter1) := split(index)
      index = index + 1
    }
    ret
  }

  def apply[T <: Data](dataType: HardType[T], bits: Array[Bits], n2: Int, n1: Int) = {
    require(bits.length == dataType.getBitsWidth * n1 * n2)
    val ret = Vec(Vec(dataType, n2), n1)
    val split = bits.grouped(dataType.getBitsWidth).toArray.map(Vec(_).as(dataType))
    var index = 0
    for (iter1 <- 0 until n1) {
      for (iter2 <- 0 until n2) {
        ret(iter1)(iter2) := split(index)
        index = index + 1
      }
    }
    ret
  }

  def apply[T <: Data](dataType: HardType[T], bits: Array[Bits], n3: Int, n2: Int, n1: Int) = {
    require(bits.length == dataType.getBitsWidth * n1 * n2 * n3)
    val ret = Vec(Vec(Vec(dataType, n3), n2), n1)
    val split = bits.grouped(dataType.getBitsWidth).toArray.map(Vec(_).as(dataType))
    var index = 0
    for (iter1 <- 0 until n1) {
      for (iter2 <- 0 until n2) {
        for (iter3 <- 0 until n3) {
          ret(iter1)(iter2)(iter3) := split(index)
          index = index + 1
        }
      }
    }
    ret
  }

  def apply[T <: Data](dataType: HardType[T], bits: Array[Bits], n4: Int, n3: Int, n2: Int, n1: Int) = {
    require(bits.length == dataType.getBitsWidth * n1 * n2 * n3 * n4)
    val ret = Vec(Vec(Vec(Vec(dataType, n4), n3), n2), n1)
    val split = bits.grouped(dataType.getBitsWidth).toArray.map(Vec(_).as(dataType))
    var index = 0
    for (iter1 <- 0 until n1) {
      for (iter2 <- 0 until n2) {
        for (iter3 <- 0 until n3) {
          for (iter4 <- 0 until n4) {
            ret(iter1)(iter2)(iter3)(iter4) := split(index)
            index = index + 1
          }
        }
      }
    }
    ret
  }

  def apply[T <: Data](dataType: HardType[T], bits: Array[Bits], n5: Int, n4: Int, n3: Int, n2: Int, n1: Int) = {
    require(bits.length == dataType.getBitsWidth * n1 * n2 * n3 * n4 * n5)
    val ret = Vec(Vec(Vec(Vec(Vec(dataType, n5), n4), n3), n2), n1)
    val split = bits.grouped(dataType.getBitsWidth).toArray.map(Vec(_).as(dataType))
    var index = 0
    for (iter1 <- 0 until n1) {
      for (iter2 <- 0 until n2) {
        for (iter3 <- 0 until n3) {
          for (iter4 <- 0 until n4) {
            for (iter5 <- 0 until n5) {
              ret(iter1)(iter2)(iter3)(iter4)(iter5) := split(index)
              index = index + 1
            }
          }
        }
      }
    }
    ret
  }
}
