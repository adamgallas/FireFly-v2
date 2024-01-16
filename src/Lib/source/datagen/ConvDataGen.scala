package datagen

import ai.djl.ndarray.index.NDIndex
import ai.djl.ndarray.{NDArray, NDList, NDManager}
import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.nn.{Block, Parameter}
import ai.djl.nn.convolutional.Conv2d
import ai.djl.training.ParameterStore
import ai.djl.training.initializer.NormalInitializer

class ConvDataGen(
                   manager: NDManager,
                   batch: Int,
                   channels: (Int, Int),
                   imgSize: (Int, Int),
                   padding: (Int, Int),
                   kernel: (Int, Int),
                   dilation: (Int, Int),
                   stride: (Int, Int),
                   xRange: (Int, Int),
                   wRange: (Int, Int),
                   bRange: (Int, Int),
                   optBias: Boolean
                 ) {

  val (outChannels, inChannels) = channels
  val x = manager.randomInteger(xRange._1, xRange._2, new Shape(batch, inChannels, imgSize._1, imgSize._2), DataType.INT8)
  //  val w = manager.randomUniform(wRange._1, wRange._2, new Shape(outChannels, inChannels, kernel._1, kernel._2)).round().toType(DataType.INT8, false)
  //  val b = manager.randomUniform(bRange._1, bRange._2, new Shape(outChannels)).round().toType(DataType.INT32, false)

  val w_ = manager.
    truncatedNormal(new Shape(outChannels, inChannels, kernel._1, kernel._2)).
    muli((wRange._2 - wRange._1) / 4.0)

  val w = w_.clip(wRange._1, wRange._2).
    round().
    toType(DataType.INT8, false)

  val b_ = manager.
    truncatedNormal(new Shape(outChannels)).
    muli((bRange._2 - bRange._1) / 4.0)

  val b = b_.clip(bRange._1, bRange._2).
    round().
    toType(DataType.INT32, false)

  val block = Conv2d.builder.
    setKernelShape(new Shape(kernel._1, kernel._2)).
    optPadding(new Shape(padding._1, padding._2)).
    optStride(new Shape(stride._1, stride._2)).
    optDilation(new Shape(dilation._1, dilation._2)).
    optBias(optBias).
    setFilters(outChannels).
    build

  block.setInitializer(new NormalInitializer, Parameter.Type.WEIGHT)
  block.initialize(manager, DataType.FLOAT32, x.getShape)

  val wParam = block.getParameters.get(0).getValue.getArray
  wParam.setRequiresGradient(false)
  wParam.set(w.toType(DataType.FLOAT32, false).toFloatArray)

  if (optBias) {
    val bParam = block.getParameters.get(1).getValue.getArray
    bParam.setRequiresGradient(false)
    bParam.set(b.toType(DataType.FLOAT32, false).toFloatArray)
  }

  val parameterStore = new ParameterStore(manager, false)
  val y = block.
    forward(parameterStore, new NDList(x.toType(DataType.FLOAT32, false)), false).
    singletonOrThrow.
    toType(DataType.INT32, false)
}
