package utils

object Array2BigInt {
  def apply(data: Array[Int], bits: Array[Int]) = {
    require(data.length == bits.length)
    (for (i <- data.indices) yield {
      BigInt(data(i)) << bits.take(i).sum
    }).sum
  }
}
