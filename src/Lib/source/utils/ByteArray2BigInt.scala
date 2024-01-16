package utils

object ByteArray2BigInt {
  def apply(data: Array[Byte]) = {
    val buffer = data.reverse.toBuffer
    buffer.prepend(0.toByte)
    BigInt(buffer.toArray)
  }
}
