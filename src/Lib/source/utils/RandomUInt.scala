package utils

import scala.util.Random

object RandomUInt {
  def apply(bitCount: Int) = {
    val bound = 1 << bitCount
    val value = Random.nextInt(bound)
    value
  }

}
