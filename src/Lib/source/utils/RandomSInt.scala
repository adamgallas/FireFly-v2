package utils

import scala.util.Random

object RandomSInt {
  def apply(bitCount: Int) = {
    val bound = 1 << bitCount
    val value = Random.nextInt(bound) - bound / 2
    value
  }

}
