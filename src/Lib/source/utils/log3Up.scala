package utils

import scala.math._

object log3Up {
  def apply(value: Int): Int = {
    math.ceil(log10(value) / log10(3.0)).toInt
  }
}