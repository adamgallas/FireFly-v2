import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.nn.pooling.Pool.{avgPool2d, maxPool2d}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.tools.BigIntToListBoolean

import java.io.{File, FileInputStream}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class ConvCfg(
               channels: (Int, Int),
               imgSize: (Int, Int),
               padding: (Int, Int),
               kernel: (Int, Int),
               dilation: (Int, Int),
               stride: (Int, Int)
             ) {
  val out_channels = channels._1
  val in_channels = channels._2
  val img_height = imgSize._1
  val img_width = imgSize._2
  val kernel_height = kernel._1
  val kernel_width = kernel._2
  val stride_height = stride._1
  val stride_width = stride._2
  val pad_height = padding._1
  val pad_width = padding._2
  val dilation_height = dilation._1
  val dilation_width = dilation._2

  def print() = {
    println(
      s"Conv " +
        s"channel:($out_channels, $in_channels), " +
        s"img:($img_height, $img_width), " +
        s"pad:($pad_height, $pad_width), " +
        s"kernel:($kernel_height, $kernel_width), " +
        s"dilate:($dilation_height, $dilation_width), " +
        s"stride:($stride_height, $stride_width)"
    )
  }
}

class TaskCfg(
               inputBits: Int,
               shortcutBits: Int,
               timeStep: Int,
               poolingType: String,
               resConnect: String,
               enableSpikeAcc: Boolean,
               enableVmem: Boolean,
               convCfg: ConvCfg
             ) {

  val in_spike_bits = inputBits
  val time_step = timeStep
  val pooling_type = poolingType
  val res_connect = resConnect
  val shortcut_spike_bits = shortcutBits
  val spike_acc = enableSpikeAcc
  val vmem_out = enableVmem
  val conv_cfg = convCfg

  def print() = {
    println(
      s"Task " +
        s"input_bits:$in_spike_bits, " +
        s"shortcut_bits:$shortcut_spike_bits, " +
        s"time_step:$time_step, " +
        s"pooling_type:$pooling_type, " +
        s"res_connect:$res_connect, " +
        s"spike_acc:$spike_acc, " +
        s"vmem_out:$vmem_out"
    )
    conv_cfg.print()
  }
}

class CoreCfg(
               parallel: (Int, Int, Int, Int),
               bus: (Int, Int, Int),
               node: String,
               bufDepth: Int
             ) {
  val parallelism = parallel
  val busChannel = bus
  val nodeType = node
  val im2colDepth = bufDepth
}

object SimUtils {
  def InputDriver(inputBits: Int)(dst: Bits, src: Array[Int]) = {
    dst #= src.reverse.foldLeft(BigInt(0))((p, b) => (p << inputBits) + b)
  }

  def WeightDriver(dst: Bits, src: Array[Int]) = {
    dst #= src.reverse.foldLeft(BigInt(0))((p, b) => (p << 8) + (b & 0xff))
  }

  def BiasThresholdDriver(dst: Bits, src: Array[Int]) = {
    dst #= src.reverse.foldLeft(BigInt(0))((p, b) => (p << 32) + (b & 0xffffffff))
  }

  def OutputDriver(outBusCh: Int, outputBits: Int)(src: Bits) = {
    var res = src.toBigInt
    val ret = ArrayBuffer[Int]()
    for (i <- 0 until outBusCh * (8 / outputBits)) {
      val low = res & ((BigInt(1) << outputBits) - 1)
      ret.append(low.toInt)
      res = res >> outputBits
    }
    ret.toArray
  }

  def sumOutputDriver(outBusCh: Int)(src: Bits) = {
    var res = src.toBigInt
    val ret = ArrayBuffer[Int]()
    for (i <- 0 until outBusCh * (8 / 4)) {
      val low = res & ((BigInt(1) << 4) - 1)
      ret.append(low.toInt)
      res = res >> 4
    }
    ret.toArray
  }

  def vmemOutDriver(outBusCh: Int)(src: Bits) = {
    var res = src.toBigInt
    val ret = ArrayBuffer[Int]()
    for (i <- 0 until outBusCh / 2) {
      val lowRaw = res & ((BigInt(1) << 16) - 1)
      val low = if (lowRaw >= (BigInt(1) << 15)) lowRaw - (BigInt(1) << 16) else lowRaw
      ret.append(low.toInt)
      res = res >> 16
    }
    ret.toArray
  }

  def gen_config_word(cfgList: List[List[Int]], core: FireFlyFasterCfgWrapper) = {

    val config0Bits = core.config0Bits.toArray
    val config1Bits = core.config1Bits.toArray
    val config2Bits = core.config2Bits.toArray
    val config3Bits = core.config3Bits.toArray
    val config4Bits = core.config4Bits.toArray
    val config5Bits = core.config5Bits.toArray
    val im2colConfig0Bits = core.im2colConfig0Bits.toArray
    val im2colConfig1Bits = core.im2colConfig1Bits.toArray
    val im2colConfig2Bits = core.im2colConfig2Bits.toArray
    val im2colConfig3Bits = core.im2colConfig3Bits.toArray
    val im2colConfig4Bits = core.im2colConfig4Bits.toArray
    val im2colConfig5Bits = core.im2colConfig5Bits.toArray
    val im2colConfig6Bits = core.im2colConfig6Bits.toArray
    val im2colConfig7Bits = core.im2colConfig7Bits.toArray
    val im2colConfig8Bits = core.im2colConfig8Bits.toArray

    var configWord0 = BigInt(0)
    for (i <- cfgList.head.indices) {
      configWord0 = configWord0 + (BigInt(cfgList.head(i)) << config0Bits(i))
    }
    var configWord1 = BigInt(0)
    for (i <- cfgList(1).indices) {
      configWord1 = configWord1 + (BigInt(cfgList(1)(i)) << config1Bits(i))
    }
    var configWord2 = BigInt(0)
    for (i <- cfgList(2).indices) {
      configWord2 = configWord2 + (BigInt(cfgList(2)(i)) << config2Bits(i))
    }
    var configWord3 = BigInt(0)
    for (i <- cfgList(3).indices) {
      configWord3 = configWord3 + (BigInt(cfgList(3)(i)) << config3Bits(i))
    }
    var configWord4 = BigInt(0)
    for (i <- cfgList(4).indices) {
      configWord4 = configWord4 + (BigInt(cfgList(4)(i)) << config4Bits(i))
    }
    var configWord5 = BigInt(0)
    for (i <- cfgList(5).indices) {
      configWord5 = configWord5 + (BigInt(cfgList(5)(i)) << config5Bits(i))
    }
    var im2colConfigWord0 = BigInt(0)
    for (i <- cfgList(6).indices) {
      im2colConfigWord0 = im2colConfigWord0 + (BigInt(cfgList(6)(i)) << im2colConfig0Bits(i))
    }
    var im2colConfigWord1 = BigInt(0)
    for (i <- cfgList(7).indices) {
      im2colConfigWord1 = im2colConfigWord1 + (BigInt(cfgList(7)(i)) << im2colConfig1Bits(i))
    }
    var im2colConfigWord2 = BigInt(0)
    for (i <- cfgList(8).indices) {
      im2colConfigWord2 = im2colConfigWord2 + (BigInt(cfgList(8)(i)) << im2colConfig2Bits(i))
    }
    var im2colConfigWord3 = BigInt(0)
    for (i <- cfgList(9).indices) {
      im2colConfigWord3 = im2colConfigWord3 + (BigInt(cfgList(9)(i)) << im2colConfig3Bits(i))
    }
    var im2colConfigWord4 = BigInt(0)
    for (i <- cfgList(10).indices) {
      im2colConfigWord4 = im2colConfigWord4 + (BigInt(cfgList(10)(i)) << im2colConfig4Bits(i))
    }
    var im2colConfigWord5 = BigInt(0)
    for (i <- cfgList(11).indices) {
      im2colConfigWord5 = im2colConfigWord5 + (BigInt(cfgList(11)(i)) << im2colConfig5Bits(i))
    }
    var im2colConfigWord6 = BigInt(0)
    for (i <- cfgList(12).indices) {
      im2colConfigWord6 = im2colConfigWord6 + (BigInt(cfgList(12)(i)) << im2colConfig6Bits(i))
    }
    var im2colConfigWord7 = BigInt(0)
    for (i <- cfgList(13).indices) {
      im2colConfigWord7 = im2colConfigWord7 + (BigInt(cfgList(13)(i)) << im2colConfig7Bits(i))
    }
    var im2colConfigWord8 = BigInt(0)
    for (i <- cfgList(14).indices) {
      im2colConfigWord8 = im2colConfigWord8 + (BigInt(cfgList(14)(i)) << im2colConfig8Bits(i))
    }
    val configWord = List(
      configWord0, configWord1, configWord2, configWord3, configWord4, configWord5,
      im2colConfigWord0, im2colConfigWord1, im2colConfigWord2, im2colConfigWord3, im2colConfigWord4, im2colConfigWord5, im2colConfigWord6, im2colConfigWord7, im2colConfigWord8
    )
    configWord
  }
}

class ModelCfg(
                coreCfg: CoreCfg,
                cfgList: List[TaskCfg]
              ) {
  val core = coreCfg
  val cfg_list = cfgList
}

object CfgParser {

  def align(cfg: TaskCfg, parallelism: (Int, Int, Int, Int)) = {
    val (m, v, n, t) = parallelism
    if (cfg.in_spike_bits == 8 && cfg.time_step == 1) {
      // classifier
      new TaskCfg(
        inputBits = 2,
        shortcutBits = cfg.shortcut_spike_bits,
        timeStep = 4,
        poolingType = cfg.pooling_type,
        resConnect = cfg.res_connect,
        enableSpikeAcc = cfg.spike_acc,
        enableVmem = cfg.vmem_out,
        convCfg = new ConvCfg(
          channels = ((cfg.conv_cfg.out_channels + m - 1) / m * m, (cfg.conv_cfg.in_channels + v - 1) / v * v),
          imgSize = (cfg.conv_cfg.img_height, cfg.conv_cfg.img_width),
          padding = (cfg.conv_cfg.pad_height, cfg.conv_cfg.pad_width),
          kernel = (cfg.conv_cfg.kernel_height, cfg.conv_cfg.kernel_width),
          dilation = (cfg.conv_cfg.dilation_height, cfg.conv_cfg.dilation_width),
          stride = (cfg.conv_cfg.stride_height, cfg.conv_cfg.stride_width)
        )
      )
    }
    else if (cfg.in_spike_bits == 8 &&
      (cfg.conv_cfg.in_channels == 1 || cfg.conv_cfg.in_channels == 2 || cfg.conv_cfg.in_channels == 3)) {
      // direct coding
      new TaskCfg(
        inputBits = 1,
        shortcutBits = cfg.shortcut_spike_bits,
        timeStep = (cfg.time_step + t - 1) / t * t,
        poolingType = cfg.pooling_type,
        resConnect = cfg.res_connect,
        enableSpikeAcc = cfg.spike_acc,
        enableVmem = cfg.vmem_out,
        convCfg = new ConvCfg(
          channels = ((cfg.conv_cfg.out_channels + m - 1) / m * m, cfg.conv_cfg.in_channels),
          imgSize = (cfg.conv_cfg.img_height, cfg.conv_cfg.img_width),
          padding = (cfg.conv_cfg.pad_height, cfg.conv_cfg.pad_width),
          kernel = (cfg.conv_cfg.kernel_height, cfg.conv_cfg.kernel_width),
          dilation = (cfg.conv_cfg.dilation_height, cfg.conv_cfg.dilation_width),
          stride = (cfg.conv_cfg.stride_height, cfg.conv_cfg.stride_width)
        )
      )
    }
    else {
      new TaskCfg(
        inputBits = cfg.in_spike_bits,
        shortcutBits = cfg.shortcut_spike_bits,
        timeStep = (cfg.time_step + t - 1) / t * t,
        poolingType = cfg.pooling_type,
        resConnect = cfg.res_connect,
        enableSpikeAcc = cfg.spike_acc,
        enableVmem = cfg.vmem_out,
        convCfg = new ConvCfg(
          channels = ((cfg.conv_cfg.out_channels + m - 1) / m * m,
            if (cfg.conv_cfg.kernel_height == 1 && cfg.conv_cfg.kernel_height == 1 && cfg.conv_cfg.in_channels < n * 16)
              n * 16
            else
              (cfg.conv_cfg.in_channels + v - 1) / v * v),
          imgSize = (cfg.conv_cfg.img_height, cfg.conv_cfg.img_width),
          padding = (cfg.conv_cfg.pad_height, cfg.conv_cfg.pad_width),
          kernel = (cfg.conv_cfg.kernel_height, cfg.conv_cfg.kernel_width),
          dilation = (cfg.conv_cfg.dilation_height, cfg.conv_cfg.dilation_width),
          stride = (cfg.conv_cfg.stride_height, cfg.conv_cfg.stride_width)
        )
      )
    }
  }

  def apply(filename: String, parallelism: (Int, Int, Int, Int), busChannel: (Int, Int, Int), bufDepth: Int) = {

    val yaml: Yaml = new Yaml(new Constructor(classOf[java.util.LinkedHashMap[String, Any]]))
    val input = new FileInputStream(new File(filename))
    val yamlMap: java.util.LinkedHashMap[String, Any] = yaml.load(input).asInstanceOf[java.util.LinkedHashMap[String, Any]]
    val nodeType = yamlMap.get("node").asInstanceOf[String]
    val taskName = yamlMap.keySet().toArray().drop(1).map(_.asInstanceOf[String])
    val cfgList = for (name <- taskName) yield {
      val map = yamlMap.get(name).asInstanceOf[java.util.LinkedHashMap[String, Any]]
      val convMap = map.get("conv_cfg").asInstanceOf[java.util.LinkedHashMap[String, Any]]

      val convCfg = new ConvCfg(
        channels = (convMap.get("out_channels").asInstanceOf[Int], convMap.get("in_channels").asInstanceOf[Int]),
        imgSize = (convMap.get("img_height").asInstanceOf[Int], convMap.get("img_width").asInstanceOf[Int]),
        padding = (
          if (convMap.get("pad_height") != null) convMap.get("pad_height").asInstanceOf[Int] else 1,
          if (convMap.get("pad_width") != null) convMap.get("pad_width").asInstanceOf[Int] else 1
        ),
        kernel = (
          if (convMap.get("kernel_height") != null) convMap.get("kernel_height").asInstanceOf[Int] else 3,
          if (convMap.get("kernel_width") != null) convMap.get("kernel_width").asInstanceOf[Int] else 3
        ),
        dilation = (
          if (convMap.get("dilate_height") != null) convMap.get("dilate_height").asInstanceOf[Int] else 1,
          if (convMap.get("dilate_width") != null) convMap.get("dilate_width").asInstanceOf[Int] else 1
        ),
        stride = (
          if (convMap.get("stride_height") != null) convMap.get("stride_height").asInstanceOf[Int] else 1,
          if (convMap.get("stride_width") != null) convMap.get("stride_width").asInstanceOf[Int] else 1
        )
      )
      val taskCfg = new TaskCfg(
        inputBits = map.get("in_spike_bits").asInstanceOf[Int],
        shortcutBits = if (map.get("shortcut_spike_bits") != null) map.get("shortcut_spike_bits").asInstanceOf[Int] else 0,
        timeStep = map.get("time_step").asInstanceOf[Int],
        poolingType = if (map.get("pooling_type") != null) map.get("pooling_type").asInstanceOf[String] else "none",
        resConnect = if (map.get("res_connect") != null) map.get("res_connect").asInstanceOf[String] else "none",
        enableSpikeAcc = if (map.get("spike_acc") != null) map.get("spike_acc").asInstanceOf[String] == "enabled" else false,
        enableVmem = if (map.get("vmem_out") != null) map.get("vmem_out").asInstanceOf[String] == "enabled" else false,
        convCfg = convCfg
      )
      align(taskCfg, parallelism)
    }

    val coreCfg = new CoreCfg(
      parallel = parallelism,
      bus = busChannel,
      node = nodeType,
      bufDepth = bufDepth
    )

    new ModelCfg(coreCfg, cfgList.toList)
  }
}
