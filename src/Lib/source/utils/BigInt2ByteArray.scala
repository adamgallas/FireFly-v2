package utils

object BigInt2ByteArray {
  def apply(data: BigInt, len: Int): Array[Byte] = {
    val dataArray = data.toByteArray.reverse
    var length = scala.math.min(len, dataArray.length)
    val dummy = if (data < 0) -1 else 0
    var result = Array.fill[Byte](len)(dummy.toByte)
    for (i <- 0 until length)
      result(i) = dataArray(i)
    result
  }
}
