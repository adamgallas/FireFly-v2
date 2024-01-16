package datagen

import ai.djl.ndarray.types.DataType
import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.index.NDIndex

object SNNNode {

  def apply(manager: NDManager, x: NDArray, threshold: Int, leaky: Boolean) = {
    val vMem = manager.zeros(x.get(0).getShape, DataType.FLOAT32)
    val spikes = manager.zeros(x.getShape, DataType.BOOLEAN)
    for (t <- 0 until x.getShape.get(0).toInt) {
      val sum = if (leaky) x.get(t).add(vMem).divi(2).floor() else x.get(t).add(vMem)
      vMem.set(sum.toFloatArray)
      val spike = vMem.gt(threshold)
      vMem.set(spike, 0f)
      spikes.set(new NDIndex(t), spike)
    }
    spikes
  }

  def apply(manager: NDManager, x: NDArray, threshold: Array[Int], leaky: Boolean) = {
    val vMem = manager.zeros(x.get(0).getShape, DataType.FLOAT32)
    val channels = x.getShape.get(1).toInt
    val spikes = manager.zeros(x.getShape, DataType.BOOLEAN)
    for (t <- 0 until x.getShape.get(0).toInt) {
      val sum = if(leaky) x.get(t).add(vMem).divi(2).floor() else x.get(t).add(vMem)
      vMem.set(sum.toFloatArray)
      for (c <- 0 until channels) {
        val spike = vMem.get(c).gt(threshold(c))
        vMem.get(c).set(spike, 0f)
        spikes.set(new NDIndex(t, c), spike)
      }
    }
    spikes
  }
}
