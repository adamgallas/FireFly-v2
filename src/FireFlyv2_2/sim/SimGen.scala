import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.nn.pooling.Pool.{avgPool2d, maxPool2d}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import spinal.core._
import datagen.{ConvDataGen, SNNNode}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.tools.BigIntToListBoolean

import java.io.{File, FileInputStream}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class CfgList4Conv(
                    coreCfg: CoreCfg,
                    cfg: TaskCfg
                  ) {

  val parallelism = coreCfg.parallelism
  val busChannels = coreCfg.busChannel
  val im2colDepth = coreCfg.im2colDepth

  val (m, v, n, t) = parallelism
  val isDirectCoding = cfg.conv_cfg.in_channels < 4

  val inChannel = if (isDirectCoding) v else cfg.conv_cfg.in_channels

  val im2colCfg = new Im2colWithTConfigGen(
    parallelism, cfg.time_step * cfg.in_spike_bits,
    (cfg.conv_cfg.out_channels, inChannel),
    (cfg.conv_cfg.img_height, cfg.conv_cfg.img_width),
    (cfg.conv_cfg.pad_height, cfg.conv_cfg.pad_width),
    (cfg.conv_cfg.kernel_height, cfg.conv_cfg.kernel_width),
    (cfg.conv_cfg.dilation_height, cfg.conv_cfg.dilation_width),
    (cfg.conv_cfg.stride_height, cfg.conv_cfg.stride_width),
    bufDepth = im2colDepth
  )

  val out_spike_bits = if (cfg.pooling_type == "avgpool" || cfg.res_connect.contains("add")) 2 else 1
  val outCol = if (cfg.pooling_type != "none") im2colCfg.conv2dRealWidth / 2 else im2colCfg.conv2dRealWidth
  val outRow = if (cfg.pooling_type != "none") im2colCfg.conv2dReadHeight / 2 else im2colCfg.conv2dReadHeight
  val outPixel = outCol * outRow
  val outElems = cfg.conv_cfg.out_channels * outPixel * cfg.time_step * out_spike_bits

  val enableConcat = false
  val enableMaxpool = cfg.pooling_type == "maxpool"
  val enableAvgpool = cfg.pooling_type == "avgpool"
  val enableResConnect = cfg.res_connect != "none"
  val enableSpikeAcc = cfg.spike_acc
  val enableVmemOut = cfg.vmem_out
  val spikeAccTwoBit = enableSpikeAcc & out_spike_bits == 2

  val sewConnectMuxSel = cfg.res_connect match {
    case "iand" => 0
    case "none" => 0
    case "add" => if (cfg.shortcut_spike_bits == 1) 1 else 2
  }
  val reduceMode = cfg.in_spike_bits match {
    case 1 => if (isDirectCoding) 1 else 3
    case 2 => 2
  }
  val reduceTimeStep = cfg.time_step * cfg.in_spike_bits / t - 1
  val dynamicTimeStep = cfg.time_step / t - 1
  val maxpoolTimeStep = cfg.time_step / t - 1

  val weightReuse = im2colCfg.weightReuse
  val weightLength = im2colCfg.weightLength

  val concatLength0 = cfg.time_step * cfg.in_spike_bits / t * inChannel / v
  val concatLength1 = 0

  val spikeLength = cfg.time_step / t * im2colCfg.outImgSize._1 * im2colCfg.outImgSize._2 - 1
  val spikeAccLength = if(cfg.spike_acc) outPixel * cfg.time_step * out_spike_bits / t - 1 else 0

  val columns = im2colCfg.outImgSize._2 * cfg.time_step / t - 1
  val packLength = if (enableMaxpool) (columns + 1) / 2 - 1 else columns
  val validLength =
    if (enableMaxpool) im2colCfg.conv2dRealWidth * cfg.time_step / t / 2 - 1
    else im2colCfg.conv2dRealWidth * cfg.time_step / t - 1

  val transposeFirstDim = cfg.time_step * cfg.in_spike_bits / t - 1
  val transposeSecondDim = inChannel / v - 1

  val pushWidth = im2colCfg.pushWidth
  val pushHeight = im2colCfg.pushHeight
  val pushHeightReserve = im2colCfg.pushHeightReserve
  val popKernelWidth = im2colCfg.popKernelWidth
  val popKernelHeight = im2colCfg.popKernelHeight
  val popHeightInc = im2colCfg.popHeightInc
  val popOffset = im2colCfg.popOffset
  val popChannels = im2colCfg.popChannels
  val popWidth = im2colCfg.popWidth
  val popHeight = im2colCfg.popHeight
  val popReuse = im2colCfg.popReuse
  val popKernelWidthAddrInc = im2colCfg.popKernelWidthAddrInc
  val popKernelHeightAddrInc = im2colCfg.popKernelHeightAddrInc
  val popWidthAddrInc = im2colCfg.popWidthAddrInc
  val popHeightAddrInc = im2colCfg.popHeightAddrInc
  val pushChannelsTimeStep = im2colCfg.pushChannelsTimeStep
  val popTimeStepAddrInc = im2colCfg.popTimeStepAddrInc
  val popTimeSteps = im2colCfg.popTimeSteps
  val pad4CoalesceShape = im2colCfg.pad4CoalesceShape
  val pad4CoalesceTail = im2colCfg.pad4CoalesceTail
  val popCoalesceBound = im2colCfg.popCoalesceBound
  val pad4ConvHead_1 = im2colCfg.pad4ConvHead_1
  val pad4ConvHead_2 = im2colCfg.pad4ConvHead_2
  val pad4ConvTail_1 = im2colCfg.pad4ConvTail_1
  val pad4ConvTail_2 = im2colCfg.pad4ConvTail_2
  val pad4ConvShape_0 = im2colCfg.pad4ConvShape_0
  val pad4ConvShape_1 = im2colCfg.pad4ConvShape_1
  val pad4ConvShape_2 = im2colCfg.pad4ConvShape_2

  val config0 = List(
    enableConcat.toInt,
    enableMaxpool.toInt,
    enableAvgpool.toInt,
    enableResConnect.toInt,
    enableSpikeAcc.toInt,
    enableVmemOut.toInt,
    spikeAccTwoBit.toInt,
    sewConnectMuxSel,
    reduceMode,
    reduceTimeStep,
    dynamicTimeStep,
    maxpoolTimeStep
  )
  val config1 = List(weightReuse, weightLength)
  val config2 = List(concatLength0, concatLength1)
  val config3 = List(spikeLength, spikeAccLength)
  val config4 = List(columns, packLength, validLength)
  val config5 = List(transposeFirstDim, transposeSecondDim)
  val im2colConfig0 = List(pushWidth, pushHeight)
  val im2colConfig1 = List(pushHeightReserve, popKernelWidth, popKernelHeight, popHeightInc, popOffset)
  val im2colConfig2 = List(popChannels, popWidth, popHeight, popReuse)
  val im2colConfig3 = List(popKernelWidthAddrInc, popKernelHeightAddrInc)
  val im2colConfig4 = List(popWidthAddrInc, popHeightAddrInc)
  val im2colConfig5 = List(pushChannelsTimeStep, popTimeStepAddrInc, popTimeSteps)
  val im2colConfig6 = List(pad4CoalesceShape, pad4CoalesceTail, popCoalesceBound)
  val im2colConfig7 = List(pad4ConvHead_1, pad4ConvHead_2, pad4ConvTail_1, pad4ConvTail_2)
  val im2colConfig8 = List(pad4ConvShape_0, pad4ConvShape_1, pad4ConvShape_2)

  val cfgList = List(
    config0, config1, config2, config3, config4, config5,
    im2colConfig0, im2colConfig1, im2colConfig2, im2colConfig3, im2colConfig4, im2colConfig5, im2colConfig6, im2colConfig7, im2colConfig8
  )
}

class DataGen4Conv(
                    manager: NDManager,
                    coreCfg: CoreCfg,
                    cfg: TaskCfg
                  ) {

  val parallelism = coreCfg.parallelism
  val busChannels = coreCfg.busChannel
  val nodeType = coreCfg.nodeType

  val (m, v, n, t) = parallelism
  val isDirectCoding = cfg.conv_cfg.in_channels < 4
  val (inBusCh, paramBusCh, outBusCh) = busChannels

  val inChannel = if (isDirectCoding) v else cfg.conv_cfg.in_channels

  val xRange = (0, 1 << cfg.in_spike_bits)
  val wRange = (-32, 32)
  val bRange = (0, 1)
  val shortCutRange = (0, 1 << cfg.shortcut_spike_bits)
  val threshold = Array.fill(cfg.conv_cfg.out_channels)(Random.nextInt(1))

  val inputPass = cfg.conv_cfg.out_channels / m
  val out_spike_bits = if (cfg.pooling_type == "avgpool" || cfg.res_connect.contains("add")) 2 else 1

  val gen = new ConvDataGen(
    manager, cfg.time_step,
    (cfg.conv_cfg.out_channels, inChannel),
    (cfg.conv_cfg.img_height, cfg.conv_cfg.img_width),
    (cfg.conv_cfg.pad_height, cfg.conv_cfg.pad_width),
    (cfg.conv_cfg.kernel_height, cfg.conv_cfg.kernel_width),
    (cfg.conv_cfg.dilation_height, cfg.conv_cfg.dilation_width),
    (cfg.conv_cfg.stride_height, cfg.conv_cfg.stride_width),
    xRange, wRange, bRange, true
  )

  val w = gen.w
  val x = gen.x
  val b = gen.b
  val y = if (isDirectCoding) {
    val y0 = gen.y.get(0)
    val y1 = gen.y.get(1)
    val y2 = gen.y.get(2)
    val y3 = gen.y.get(3)
    val sum0 = y0.add(y1.muli(4))
    val sum1 = y2.add(y3.muli(4))
    val sum = sum0.add(sum1.muli(16))
    sum.expandDims(0).repeat(0, cfg.time_step)
  }
  else {
    gen.y
  }

  val s = SNNNode(manager, y, threshold, leaky = nodeType == "lif")
  val qMaxpool = if (cfg.pooling_type == "maxpool") maxPool2d(s.toType(DataType.FLOAT32, true), new Shape(2, 2), new Shape(2, 2), new Shape(0, 0), false).toType(DataType.BOOLEAN, false) else null
  val qAvgpool = if (cfg.pooling_type == "avgpool") avgPool2d(s.toType(DataType.FLOAT32, true), new Shape(2, 2), new Shape(2, 2), new Shape(0, 0), false, false).muli(4).clip(0.0, 3.0) else null
  val p = (if (cfg.pooling_type == "maxpool") qMaxpool else if (cfg.pooling_type == "avgpool") qAvgpool else s).toType(DataType.INT32, false)
  val shortcut = if (cfg.res_connect != "none") manager.randomInteger(shortCutRange._1, shortCutRange._2, p.getShape, DataType.INT32) else null
  val r =
    if (cfg.res_connect.contains("add")) p.addi(shortcut).clip(0, 3)
    else if (cfg.res_connect.contains("iand")) p.toType(DataType.BOOLEAN, false).logicalNot().logicalAnd(shortcut.toType(DataType.BOOLEAN, false))
    else p

  val rSum = r.sum(Array(0, 2, 3)).toType(DataType.INT32, false).toIntArray
  val rSumLow = rSum.map(x => x & 0xf).grouped(m).toArray
  val rSumHigh = rSum.map(x => (x >> 4) & 0xf).grouped(m).toArray
  val rSumSeq = (rSumLow, rSumHigh).zipped.flatMap((l, h) => l ++ h)
  val rSumStream = rSumSeq.grouped(outBusCh * 8 / 4).toArray

  val xStream = x.
    toType(DataType.INT32, false).
    reshape(new Shape(
      cfg.time_step / (t / cfg.in_spike_bits),
      t / cfg.in_spike_bits,
      inChannel / v, v, x.getShape.get(2), x.getShape.get(3))).
    transpose(4, 5, 2, 0, 3, 1).
    flatten().
    toIntArray.
    grouped(inBusCh * (8 / cfg.in_spike_bits)).
    toArray

  val yStream = y.
    reshape(new Shape(cfg.time_step / t, t, cfg.conv_cfg.out_channels / m, m, y.getShape.get(2), y.getShape.get(3))).
    transpose(2, 4, 5, 0, 3, 1).
    toIntArray.
    grouped(outBusCh / 2).
    toArray

  val wStream = w.
    reshape(new Shape(cfg.conv_cfg.out_channels / m, m, inChannel / v, v, cfg.conv_cfg.kernel_height * cfg.conv_cfg.kernel_width)).
    transpose(0, 4, 2, 1, 3).
    toType(DataType.INT32, false).
    toIntArray.
    grouped(paramBusCh).
    toArray

  val btStream = (b.toIntArray, threshold).zipped.map((x, y) => (x & 0xfff) + ((y & 0xffff) << 16)).
    grouped(paramBusCh / 4).
    toArray

  val shortCutStream = if (cfg.res_connect != "none") shortcut.
    reshape(new Shape(cfg.time_step / (t / cfg.shortcut_spike_bits), t / cfg.shortcut_spike_bits, cfg.conv_cfg.out_channels / m, m, shortcut.getShape.get(2), shortcut.getShape.get(3))).
    transpose(2, 4, 5, 0, 3, 1).
    toIntArray.
    grouped(inBusCh * (8 / cfg.shortcut_spike_bits)).
    toArray else null

  val rStream = r.toType(DataType.INT32, false).
    reshape(new Shape(cfg.time_step / (t / out_spike_bits), t / out_spike_bits, cfg.conv_cfg.out_channels / m, m, r.getShape.get(2), r.getShape.get(3))).
    transpose(2, 4, 5, 0, 3, 1).
    toIntArray.
    grouped(outBusCh * (8 / out_spike_bits)).
    toArray
}