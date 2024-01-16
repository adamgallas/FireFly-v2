package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object Reshape {

  def apply[TI <: Data, TO <: Data](input: TI, outputType: TO, n: Int) = {
    val bitsArray = GetBitsArray(input)
    BitsArray2Vec(outputType, bitsArray, n)
  }

  def apply[TI <: Data, TO <: Data](input: TI, outputType: TO, n: (Int, Int)) = {
    val bitsArray = GetBitsArray(input)
    BitsArray2Vec(outputType, bitsArray, n._1, n._2)
  }

  def apply[TI <: Data, TO <: Data](input: TI, outputType: TO, n: (Int, Int, Int)) = {
    val bitsArray = GetBitsArray(input)
    BitsArray2Vec(outputType, bitsArray, n._1, n._2, n._3)
  }

  def apply[TI <: Data, TO <: Data](input: TI, outputType: TO, n: (Int, Int, Int, Int)) = {
    val bitsArray = GetBitsArray(input)
    BitsArray2Vec(outputType, bitsArray, n._1, n._2, n._3, n._4)
  }

  def apply[TI <: Data, TO <: Data](input: TI, outputType: TO, n: (Int, Int, Int, Int, Int)) = {
    val bitsArray = GetBitsArray(input)
    BitsArray2Vec(outputType, bitsArray, n._1, n._2, n._3, n._4, n._5)
  }
}
