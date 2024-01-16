package utils

object ShortArray2BigInt {
  def apply(data: Array[Short]) = {
    val buffer = data.flatMap(s => List(s.toByte, (s >> 8).toByte)).toArray.reverse.toBuffer
    buffer.prepend(0.toByte)
    BigInt(buffer.toArray)
  }
}
