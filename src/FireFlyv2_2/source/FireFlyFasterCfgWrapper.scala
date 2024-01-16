import fifos.StreamFifoHighPerf
import im2col.Im2col3x3
import spinal.core
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SlaveFactory, AxiLite4SpecRenamer}
import transfer.OnOffChipDataTransfer

import scala.language.postfixOps
import org.yaml.snakeyaml.{DumperOptions, Yaml}

import java.io.{File, FileWriter}
import java.util
import scala.collection.mutable.ArrayBuffer

case class FireFlyFasterCfgWrapper(
                                    ddrClk: ClockDomain,
                                    parallelism: (Int, Int, Int, Int),
                                    im2colDepth: Int,
                                    im2colBufTech: String,
                                    weightDepth: Int,
                                    inputStreamBytes: Int,
                                    paramStreamBytes: Int,
                                    outputStreamBytes: Int,
                                    nodeType: String
                                  ) extends Component {
  val (m, v, n, t) = parallelism
  val (mr, vr, nr) = (m / 4, v / 4, n)
  val io = new Bundle {
    val inputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val auxInputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val params = slave(Stream(Fragment(Bits(paramStreamBytes * 8 bits))))
    val outputs = master(Stream(Bits(outputStreamBytes * 8 bits)))
  }

  val firefly = FireFlyFasterBareMetal(
    ddrClk,
    parallelism,
    im2colDepth,
    im2colBufTech,
    weightDepth,
    inputStreamBytes,
    paramStreamBytes,
    outputStreamBytes,
    nodeType
  )

  val coreStatus = firefly.status.toIo()

  io.inputs >> firefly.io.inputs
  io.auxInputs >> firefly.io.auxInputs
  io.params >> firefly.io.params
  firefly.io.outputs >> io.outputs


  val configWord0 = new Bundle {
    val enableConcat = Bool()
    val enableMaxpool = Bool()
    val enableAvgpool = Bool()
    val enableResConnect = Bool()
    val enableSpikeAcc = Bool()
    val enableVmemOut = Bool()
    val spikeAccTwoBit = Bool()
    val sewConnectMuxSel = UInt(2 bits)
    val reduceMode = UInt(2 bits)
    val reduceTimeStep = UInt(4 bits)
    val dynamicTimeStep = UInt(4 bits)
    val maxpoolTimeStep = UInt(4 bits)
  }

  val configWord1 = new Bundle {
    val weightReuse = UInt(firefly.cfgMisc.weightReuse.getWidth bits)
    val weightLength = UInt(firefly.cfgMisc.weightLength.getWidth bits)
  }

  val configWord2 = new Bundle {
    val concatLength0 = UInt(firefly.cfgMisc.concatLength.head.getWidth bits)
    val concatLength1 = UInt(firefly.cfgMisc.concatLength(1).getWidth bits)
  }

  val configWord3 = new Bundle {
    val spikeLength = UInt(firefly.psumVmemCfg.spikeLength.getWidth bits)
    val spikeAccLength = UInt(firefly.postSpikeCfg.spikeAccLength.getWidth bits)
  }

  val configWord4 = new Bundle {
    val columns = UInt(firefly.postSpikeCfg.columns.getWidth bits)
    val packLength = UInt(firefly.postSpikeCfg.packLength.getWidth bits)
    val validLength = UInt(firefly.postSpikeCfg.validLength.getWidth bits)
  }

  val configWord5 = new Bundle {
    val transposeFirstDim = UInt(10 bits)
    val transposeSecondDim = UInt(10 bits)
  }

  val im2colConfigWord0 = new Bundle {
    val pushWidth = UInt(firefly.coreCfg.pushWidth.getWidth bits)
    val pushHeight = UInt(firefly.coreCfg.pushHeight.getWidth bits)
  }

  val im2colConfigWord1 = new Bundle {
    val pushHeightReserve = UInt(firefly.coreCfg.pushHeightReserve.getWidth bits)
    val popKernelWidth = UInt(firefly.coreCfg.popKernelWidth.getWidth bits)
    val popKernelHeight = UInt(firefly.coreCfg.popKernelHeight.getWidth bits)
    val popHeightInc = UInt(firefly.coreCfg.popHeightInc.getWidth bits)
    val popOffset = UInt(firefly.coreCfg.popOffset.getWidth bits)
  }

  val im2colConfigWord2 = new Bundle {
    val popChannels = UInt(firefly.coreCfg.popChannels.getWidth bits)
    val popWidth = UInt(firefly.coreCfg.popWidth.getWidth bits)
    val popHeight = UInt(firefly.coreCfg.popHeight.getWidth bits)
    val popReuse = UInt(firefly.coreCfg.popReuse.getWidth bits)
  }

  val im2colConfigWord3 = new Bundle {
    val popKernelWidthAddrInc = UInt(firefly.coreCfg.popKernelWidthAddrInc.getWidth bits)
    val popKernelHeightAddrInc = UInt(firefly.coreCfg.popKernelHeightAddrInc.getWidth bits)
  }

  val im2colConfigWord4 = new Bundle {
    val popWidthAddrInc = UInt(firefly.coreCfg.popWidthAddrInc.getWidth bits)
    val popHeightAddrInc = UInt(firefly.coreCfg.popHeightAddrInc.getWidth bits)
  }

  val im2colConfigWord5 = new Bundle {
    val pushChannelsTimeStep = UInt(firefly.coreCfg.pushChannelsTimeStep.getWidth bits)
    val popTimeStepInc = UInt(firefly.coreCfg.popTimeStepInc.getWidth bits)
    val timeStep = UInt(firefly.coreCfg.timeStep.getWidth bits)
  }

  val im2colConfigWord6 = new Bundle {
    val pad4CoalesceShape = UInt(firefly.coreCfg.pad4CoalesceShape.getWidth bits)
    val pad4CoalesceTail = UInt(firefly.coreCfg.pad4CoalesceTail.getWidth bits)
    val popCoalesceBound = UInt(firefly.coreCfg.popCoalesceBound.getWidth bits)
  }

  val im2colConfigWord7 = new Bundle {
    val pad4ConvHead_1 = UInt(firefly.coreCfg.pad4ConvHead_1.getWidth bits)
    val pad4ConvHead_2 = UInt(firefly.coreCfg.pad4ConvHead_2.getWidth bits)
    val pad4ConvTail_1 = UInt(firefly.coreCfg.pad4ConvTail_1.getWidth bits)
    val pad4ConvTail_2 = UInt(firefly.coreCfg.pad4ConvTail_2.getWidth bits)
  }

  val im2colConfigWord8 = new Bundle {
    val pad4ConvShape_0 = UInt(firefly.coreCfg.pad4ConvShape_0.getWidth bits)
    val pad4ConvShape_1 = UInt(firefly.coreCfg.pad4ConvShape_1.getWidth bits)
    val pad4ConvShape_2 = UInt(firefly.coreCfg.pad4ConvShape_2.getWidth bits)
  }

  val configList = new Bundle {
    val config0 = in Bits (32 bits)
    val config1 = in Bits (32 bits)
    val config2 = in Bits (32 bits)
    val config3 = in Bits (32 bits)
    val config4 = in Bits (32 bits)
    val config5 = in Bits (32 bits)
    val im2colConfig0 = in Bits (32 bits)
    val im2colConfig1 = in Bits (32 bits)
    val im2colConfig2 = in Bits (32 bits)
    val im2colConfig3 = in Bits (32 bits)
    val im2colConfig4 = in Bits (32 bits)
    val im2colConfig5 = in Bits (32 bits)
    val im2colConfig6 = in Bits (32 bits)
    val im2colConfig7 = in Bits (32 bits)
    val im2colConfig8 = in Bits (32 bits)
  }

  configWord0.assignFromBits(configList.config0.take(configWord0.getBitsWidth))
  configWord1.assignFromBits(configList.config1.take(configWord1.getBitsWidth))
  configWord2.assignFromBits(configList.config2.take(configWord2.getBitsWidth))
  configWord3.assignFromBits(configList.config3.take(configWord3.getBitsWidth))
  configWord4.assignFromBits(configList.config4.take(configWord4.getBitsWidth))
  configWord5.assignFromBits(configList.config5.take(configWord5.getBitsWidth))
  im2colConfigWord0.assignFromBits(configList.im2colConfig0.take(im2colConfigWord0.getBitsWidth))
  im2colConfigWord1.assignFromBits(configList.im2colConfig1.take(im2colConfigWord1.getBitsWidth))
  im2colConfigWord2.assignFromBits(configList.im2colConfig2.take(im2colConfigWord2.getBitsWidth))
  im2colConfigWord3.assignFromBits(configList.im2colConfig3.take(im2colConfigWord3.getBitsWidth))
  im2colConfigWord4.assignFromBits(configList.im2colConfig4.take(im2colConfigWord4.getBitsWidth))
  im2colConfigWord5.assignFromBits(configList.im2colConfig5.take(im2colConfigWord5.getBitsWidth))
  im2colConfigWord6.assignFromBits(configList.im2colConfig6.take(im2colConfigWord6.getBitsWidth))
  im2colConfigWord7.assignFromBits(configList.im2colConfig7.take(im2colConfigWord7.getBitsWidth))
  im2colConfigWord8.assignFromBits(configList.im2colConfig8.take(im2colConfigWord8.getBitsWidth))

  val configMap0 = new java.util.LinkedHashMap[String, Int]()
  val config0Bits = ArrayBuffer[Int]()
  var accBits = 0
  for (elem <- configWord0.elements) {
    configMap0.put(elem._1, accBits)
    config0Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val configMap1 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val config1Bits = ArrayBuffer[Int]()
  for (elem <- configWord1.elements) {
    configMap1.put(elem._1, accBits)
    config1Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val configMap2 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val config2Bits = ArrayBuffer[Int]()
  for (elem <- configWord2.elements) {
    configMap2.put(elem._1, accBits)
    config2Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val configMap3 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val config3Bits = ArrayBuffer[Int]()
  for (elem <- configWord3.elements) {
    configMap3.put(elem._1, accBits)
    config3Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val configMap4 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val config4Bits = ArrayBuffer[Int]()
  for (elem <- configWord4.elements) {
    configMap4.put(elem._1, accBits)
    config4Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val configMap5 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val config5Bits = ArrayBuffer[Int]()
  for (elem <- configWord5.elements) {
    configMap5.put(elem._1, accBits)
    config5Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap0 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig0Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord0.elements) {
    im2colConfigMap0.put(elem._1, accBits)
    im2colConfig0Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap1 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig1Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord1.elements) {
    im2colConfigMap1.put(elem._1, accBits)
    im2colConfig1Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap2 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig2Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord2.elements) {
    im2colConfigMap2.put(elem._1, accBits)
    im2colConfig2Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap3 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig3Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord3.elements) {
    im2colConfigMap3.put(elem._1, accBits)
    im2colConfig3Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap4 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig4Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord4.elements) {
    im2colConfigMap4.put(elem._1, accBits)
    im2colConfig4Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap5 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig5Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord5.elements) {
    im2colConfigMap5.put(elem._1, accBits)
    im2colConfig5Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap6 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig6Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord6.elements) {
    im2colConfigMap6.put(elem._1, accBits)
    im2colConfig6Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap7 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig7Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord7.elements) {
    im2colConfigMap7.put(elem._1, accBits)
    im2colConfig7Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val im2colConfigMap8 = new java.util.LinkedHashMap[String, Int]()
  accBits = 0
  val im2colConfig8Bits = ArrayBuffer[Int]()
  for (elem <- im2colConfigWord8.elements) {
    im2colConfigMap8.put(elem._1, accBits)
    im2colConfig8Bits.append(accBits)
    accBits += elem._2.getBitsWidth
  }
  val totalMap = new java.util.LinkedHashMap[String, java.util.LinkedHashMap[String, Int]]()
  totalMap.put("config0", configMap0)
  totalMap.put("config1", configMap1)
  totalMap.put("config2", configMap2)
  totalMap.put("config3", configMap3)
  totalMap.put("config4", configMap4)
  totalMap.put("config5", configMap5)
  totalMap.put("im2colConfig0", im2colConfigMap0)
  totalMap.put("im2colConfig1", im2colConfigMap1)
  totalMap.put("im2colConfig2", im2colConfigMap2)
  totalMap.put("im2colConfig3", im2colConfigMap3)
  totalMap.put("im2colConfig4", im2colConfigMap4)
  totalMap.put("im2colConfig5", im2colConfigMap5)
  totalMap.put("im2colConfig6", im2colConfigMap6)
  totalMap.put("im2colConfig7", im2colConfigMap7)
  totalMap.put("im2colConfig8", im2colConfigMap8)

  val options = new DumperOptions()
  options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
  options.setPrettyFlow(true)
  val yml = new Yaml(options)
  val ymlString = yml.dump(totalMap)
  val ymlFile = new File("AXIFireFlyFasterConfig.yaml")
  val ymlFileWriter = new FileWriter(ymlFile)
  ymlFileWriter.write(ymlString)
  ymlFileWriter.close()

  firefly.cfgMisc.isConcat := RegNext(configWord0.enableConcat)
  firefly.cfgMisc.concatLength(0) := RegNext(configWord2.concatLength0)
  firefly.cfgMisc.concatLength(1) := RegNext(configWord2.concatLength1)
  firefly.cfgMisc.weightReuse := RegNext(configWord1.weightReuse)
  firefly.cfgMisc.weightLength := RegNext(configWord1.weightLength)
  firefly.cfgMisc.transposeFirstDim := RegNext(configWord5.transposeFirstDim)
  firefly.cfgMisc.transposeSecondDim := RegNext(configWord5.transposeSecondDim)

  firefly.psumVmemCfg.rPass := RegNext(configWord0.reduceTimeStep)
  firefly.psumVmemCfg.tPass := RegNext(configWord0.dynamicTimeStep)
  firefly.psumVmemCfg.spikeLength := RegNext(configWord3.spikeLength)
  firefly.psumVmemCfg.reduceMode := RegNext(configWord0.reduceMode)

  firefly.postSpikeCfg.columns := RegNext(configWord4.columns)
  firefly.postSpikeCfg.packLength := RegNext(configWord4.packLength)
  firefly.postSpikeCfg.validLength := RegNext(configWord4.validLength)
  firefly.postSpikeCfg.muxSel := RegNext(configWord0.sewConnectMuxSel)
  firefly.postSpikeCfg.enableSew := RegNext(configWord0.enableResConnect)
  firefly.postSpikeCfg.enableMaxpool := RegNext(configWord0.enableMaxpool)
  firefly.postSpikeCfg.enableAvgPool := RegNext(configWord0.enableAvgpool)
  firefly.postSpikeCfg.enableSpikeAcc := RegNext(configWord0.enableSpikeAcc)
  firefly.postSpikeCfg.enableVmemOut := RegNext(configWord0.enableVmemOut)
  firefly.postSpikeCfg.spikeAccTwoBit := RegNext(configWord0.spikeAccTwoBit)
  firefly.postSpikeCfg.spikeAccLength := RegNext(configWord3.spikeAccLength)
  firefly.postSpikeCfg.tPass := RegNext(configWord0.maxpoolTimeStep)

  firefly.coreCfg.pushWidth := im2colConfigWord0.pushWidth
  firefly.coreCfg.pushHeight := im2colConfigWord0.pushHeight
  firefly.coreCfg.pushHeightReserve := im2colConfigWord1.pushHeightReserve
  firefly.coreCfg.popKernelWidth := im2colConfigWord1.popKernelWidth
  firefly.coreCfg.popKernelHeight := im2colConfigWord1.popKernelHeight
  firefly.coreCfg.popHeightInc := im2colConfigWord1.popHeightInc
  firefly.coreCfg.popOffset := im2colConfigWord1.popOffset
  firefly.coreCfg.popChannels := im2colConfigWord2.popChannels
  firefly.coreCfg.popWidth := im2colConfigWord2.popWidth
  firefly.coreCfg.popHeight := im2colConfigWord2.popHeight
  firefly.coreCfg.popReuse := im2colConfigWord2.popReuse
  firefly.coreCfg.popKernelWidthAddrInc := im2colConfigWord3.popKernelWidthAddrInc
  firefly.coreCfg.popKernelHeightAddrInc := im2colConfigWord3.popKernelHeightAddrInc
  firefly.coreCfg.popWidthAddrInc := im2colConfigWord4.popWidthAddrInc
  firefly.coreCfg.popHeightAddrInc := im2colConfigWord4.popHeightAddrInc
  firefly.coreCfg.pushChannelsTimeStep := im2colConfigWord5.pushChannelsTimeStep
  firefly.coreCfg.popTimeStepInc := im2colConfigWord5.popTimeStepInc
  firefly.coreCfg.timeStep := im2colConfigWord5.timeStep
  firefly.coreCfg.pad4CoalesceShape := im2colConfigWord6.pad4CoalesceShape
  firefly.coreCfg.pad4CoalesceTail := im2colConfigWord6.pad4CoalesceTail
  firefly.coreCfg.popCoalesceBound := im2colConfigWord6.popCoalesceBound
  firefly.coreCfg.pad4ConvHead_1 := im2colConfigWord7.pad4ConvHead_1
  firefly.coreCfg.pad4ConvHead_2 := im2colConfigWord7.pad4ConvHead_2
  firefly.coreCfg.pad4ConvTail_1 := im2colConfigWord7.pad4ConvTail_1
  firefly.coreCfg.pad4ConvTail_2 := im2colConfigWord7.pad4ConvTail_2
  firefly.coreCfg.pad4ConvShape_0 := im2colConfigWord8.pad4ConvShape_0
  firefly.coreCfg.pad4ConvShape_1 := im2colConfigWord8.pad4ConvShape_1
  firefly.coreCfg.pad4ConvShape_2 := im2colConfigWord8.pad4ConvShape_2
}

object FireFlyFasterCfgWrapper extends App {
  SpinalVerilog(
    new FireFlyFasterCfgWrapper(
      ClockDomain.external("ddrClk"),
      (16, 16, 8, 4),
      4096 * 8,
      "ultra",
      1024,
      8,
      16,
      8,
      "if"
    )
  )
}