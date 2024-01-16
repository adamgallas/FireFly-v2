import ai.djl.ndarray.NDManager
import ai.djl.ndarray.types.{DataType, Shape}
import datagen.ConvDataGen

class SpikeWeightCalcWithTDataGen(
                                   manager: NDManager,
                                   parallelism: (Int, Int, Int, Int),
                                   timeStep: Int,
                                   channels: (Int, Int),
                                   imgSize: (Int, Int),
                                   padding: (Int, Int),
                                   kernel: (Int, Int),
                                   dilation: (Int, Int),
                                   stride: (Int, Int)
                                 ) {
  val config = new Im2colWithTConfigGen(
    parallelism, timeStep, channels, imgSize,
    padding, kernel, dilation, stride
  )

  val xRange = (0, 2)
  val wRange = (-32, 32)
  val bRange = (0, 1)

  val gen = new ConvDataGen(
    manager, timeStep, channels, imgSize,
    padding, kernel, dilation, stride,
    xRange, wRange, bRange, true
  )

  println(gen.x.getShape)
  println(gen.y.getShape)

  val xStream = gen.x.
    toType(DataType.BOOLEAN, false).
    reshape(new Shape(timeStep / config.t, config.t, config.inChannel, config.inImgSize._1, config.inImgSize._2)).
    transpose(3, 4, 0, 2, 1).
    flatten().
    toBooleanArray.
    grouped(config.t).
    map(_.reverse.foldLeft(0)((i, b) => (i << 1) + (if (b) 1 else 0))).
    toArray.
    grouped(config.v).
    toArray

  val wStream = gen.w.
    reshape(new Shape(config.outChannel / config.m, config.m, config.inChannel / config.v, config.v, kernel._1 * kernel._2)).
    transpose(0, 4, 2, 1, 3).
    toType(DataType.INT32, false).
    toIntArray.
    grouped(config.m * config.v).toArray.
    map(_.grouped(config.v).toArray)

  val bStream = gen.b.
    toType(DataType.INT32, false).
    toIntArray.
    grouped(config.m).toArray

  val yStream = gen.y.
    reshape(new Shape(timeStep / config.t, config.t, config.outChannel / config.m, config.m, config.outImgSize._1, config.outImgSize._2 / config.n, config.n)).
    transpose(2, 4, 5, 0, 6, 3, 1).
    toIntArray.
    grouped(config.m * config.t).toArray.
    map(_.grouped(config.m).toArray)
}
