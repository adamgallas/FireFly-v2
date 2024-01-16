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

case class AXIFireFlyFaster(
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

  val io = new Bundle {
    val ctrl = slave(AxiLite4(32, 32))
    val inputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val auxInputs = slave(Stream(Bits(inputStreamBytes * 8 bits)))
    val params = slave(Stream(Fragment(Bits(paramStreamBytes * 8 bits))))
    val outputs = master(Stream(Fragment(Bits(outputStreamBytes * 8 bits))))

    val inputsCmd = master(Stream(Bits(72 bits)))
    val auxInputsCmd = master(Stream(Bits(72 bits)))
    val outputsCmd = master(Stream(Bits(72 bits)))
  }
  noIoPrefix()

  AxiLite4SpecRenamer(io.ctrl)
  utils.AxiStreamSpecRenamer(io.inputs)
  utils.AxiStreamSpecRenamer(io.auxInputs)
  utils.AxiStreamSpecRenamer(io.params)
  utils.AxiStreamSpecRenamer(io.outputs)
  utils.AxiStreamSpecRenamer(io.inputsCmd)
  utils.AxiStreamSpecRenamer(io.auxInputsCmd)
  utils.AxiStreamSpecRenamer(io.outputsCmd)

  val firefly = FireFlyFasterCfgWrapper(
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

  val inputsStartPulse = Bool() setAsReg() init false
  val inputsBaseAddr = UInt(32 bits) setAsReg() init 0
  val inputsCmdBnd_0 = UInt(10 bits) setAsReg() init 0
  val inputsCmdBnd_1 = UInt(10 bits) setAsReg() init 0
  val inputsCmdInc_0 = UInt(22 bits) setAsReg() init 0
  val inputsCmdInc_1 = UInt(22 bits) setAsReg() init 0
  val inputsCmdFixLen = UInt(22 bits) setAsReg() init 0
  val inputsCmdGen = utils.FixedLenStridedAddrCmdGen(
    widthOfBnd = List(10, 10),
    widthOfInc = List(22, 22),
    addrWidth = 22, lenWidth = 22
  )
  inputsCmdGen.io.bound(0) := inputsCmdBnd_0
  inputsCmdGen.io.bound(1) := inputsCmdBnd_1
  inputsCmdGen.io.inc(0) := inputsCmdInc_0
  inputsCmdGen.io.inc(1) := inputsCmdInc_1
  inputsCmdGen.io.fixLen := inputsCmdFixLen
  inputsCmdGen.io.startPulse := inputsStartPulse

  val auxInputsStartPulse = Bool() setAsReg() init false
  val auxInputsBaseAddr = UInt(32 bits) setAsReg() init 0
  val auxInputsCmdBnd_0 = UInt(10 bits) setAsReg() init 0
  val auxInputsCmdBnd_1 = UInt(10 bits) setAsReg() init 0
  val auxInputsCmdInc_0 = UInt(22 bits) setAsReg() init 0
  val auxInputsCmdInc_1 = UInt(22 bits) setAsReg() init 0
  val auxInputsCmdFixLen = UInt(22 bits) setAsReg() init 0
  val auxInputsCmdGen = utils.FixedLenStridedAddrCmdGen(
    widthOfBnd = List(10, 10),
    widthOfInc = List(22, 22),
    addrWidth = 22, lenWidth = 22
  )
  auxInputsCmdGen.io.bound(0) := auxInputsCmdBnd_0
  auxInputsCmdGen.io.bound(1) := auxInputsCmdBnd_1
  auxInputsCmdGen.io.inc(0) := auxInputsCmdInc_0
  auxInputsCmdGen.io.inc(1) := auxInputsCmdInc_1
  auxInputsCmdGen.io.fixLen := auxInputsCmdFixLen
  auxInputsCmdGen.io.startPulse := auxInputsStartPulse

  val outputsStartPulse = Bool() setAsReg() init false
  val outputsBaseAddr = UInt(32 bits) setAsReg() init 0
  val outputsCmdBnd_0 = UInt(10 bits) setAsReg() init 0
  val outputsCmdBnd_1 = UInt(10 bits) setAsReg() init 0
  val outputsCmdInc_0 = UInt(22 bits) setAsReg() init 0
  val outputsCmdInc_1 = UInt(22 bits) setAsReg() init 0
  val outputsCmdFixLen = UInt(22 bits) setAsReg() init 0
  val outputsCmdGen = utils.FixedLenStridedAddrCmdGen(
    widthOfBnd = List(10, 10),
    widthOfInc = List(22, 22),
    addrWidth = 22, lenWidth = 22
  )
  outputsCmdGen.io.bound(0) := outputsCmdBnd_0
  outputsCmdGen.io.bound(1) := outputsCmdBnd_1
  outputsCmdGen.io.inc(0) := outputsCmdInc_0
  outputsCmdGen.io.inc(1) := outputsCmdInc_1
  outputsCmdGen.io.fixLen := outputsCmdFixLen
  outputsCmdGen.io.startPulse := outputsStartPulse

  io.inputsCmd << utils.GenAxiDataMoverCmd(inputsCmdGen.io.cmd, inputsBaseAddr, eof = True)
  io.auxInputsCmd << utils.GenAxiDataMoverCmd(auxInputsCmdGen.io.cmd, auxInputsBaseAddr)
  io.outputsCmd << utils.GenAxiDataMoverCmd(outputsCmdGen.io.cmd, outputsBaseAddr, eof = True)

  val configCtrl = new AxiLite4SlaveFactory(io.ctrl)
  configCtrl.write(inputsBaseAddr, 0x00, 0)
  configCtrl.write(inputsCmdBnd_0, 0x04, 0)
  configCtrl.write(inputsCmdInc_0, 0x04, 10)
  configCtrl.write(inputsCmdBnd_1, 0x08, 0)
  configCtrl.write(inputsCmdInc_1, 0x08, 10)
  configCtrl.write(inputsCmdFixLen, 0x0C, 0)

  configCtrl.write(auxInputsBaseAddr, 0x10, 0)
  configCtrl.write(auxInputsCmdBnd_0, 0x14, 0)
  configCtrl.write(auxInputsCmdInc_0, 0x14, 10)
  configCtrl.write(auxInputsCmdBnd_1, 0x18, 0)
  configCtrl.write(auxInputsCmdInc_1, 0x18, 10)
  configCtrl.write(auxInputsCmdFixLen, 0x1C, 0)

  configCtrl.write(outputsBaseAddr, 0x20, 0)
  configCtrl.write(outputsCmdBnd_0, 0x24, 0)
  configCtrl.write(outputsCmdInc_0, 0x24, 10)
  configCtrl.write(outputsCmdBnd_1, 0x28, 0)
  configCtrl.write(outputsCmdInc_1, 0x28, 10)
  configCtrl.write(outputsCmdFixLen, 0x2C, 0)

  configCtrl.write(inputsStartPulse, 0x30, 0)
  configCtrl.write(auxInputsStartPulse, 0x30, 8)
  configCtrl.write(outputsStartPulse, 0x30, 16)
  inputsStartPulse.clear()
  auxInputsStartPulse.clear()
  outputsStartPulse.clear()

  val configWord0 = Bits(32 bits) setAsReg() init 0
  val configWord1 = Bits(32 bits) setAsReg() init 0
  val configWord2 = Bits(32 bits) setAsReg() init 0
  val configWord3 = Bits(32 bits) setAsReg() init 0
  val configWord4 = Bits(32 bits) setAsReg() init 0
  val configWord5 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord0 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord1 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord2 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord3 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord4 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord5 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord6 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord7 = Bits(32 bits) setAsReg() init 0
  val im2colConfigWord8 = Bits(32 bits) setAsReg() init 0

  configCtrl.write(configWord0, 0x40, 0)
  configCtrl.write(configWord1, 0x44, 0)
  configCtrl.write(configWord2, 0x48, 0)
  configCtrl.write(configWord3, 0x4C, 0)
  configCtrl.write(configWord4, 0x50, 0)
  configCtrl.write(configWord5, 0x54, 0)
  configCtrl.write(im2colConfigWord0, 0x60, 0)
  configCtrl.write(im2colConfigWord1, 0x64, 0)
  configCtrl.write(im2colConfigWord2, 0x68, 0)
  configCtrl.write(im2colConfigWord3, 0x6C, 0)
  configCtrl.write(im2colConfigWord4, 0x70, 0)
  configCtrl.write(im2colConfigWord5, 0x74, 0)
  configCtrl.write(im2colConfigWord6, 0x78, 0)
  configCtrl.write(im2colConfigWord7, 0x7C, 0)
  configCtrl.write(im2colConfigWord8, 0x80, 0)

  io.inputs >> firefly.io.inputs
  io.auxInputs >> firefly.io.auxInputs
  io.params >> firefly.io.params

  val outputLastCnt = UInt(22 bits) setAsReg() init 0
  val outputLastCntOvf = outputLastCnt === (outputsCmdFixLen >> log2Up(outputStreamBytes)) - 1
  when(io.outputs.fire) {
    outputLastCnt := outputLastCnt + 1
    when(outputLastCntOvf) {
      outputLastCnt := 0
    }
  }

  io.outputs.arbitrationFrom(firefly.io.outputs)
  io.outputs.last := outputLastCntOvf
  io.outputs.payload := firefly.io.outputs.payload

  val inputsCnt = UInt(24 bits) setAsReg() init 0
  val auxInputsCnt = UInt(24 bits) setAsReg() init 0
  val outputsCnt = UInt(24 bits) setAsReg() init 0
  val paramsCnt = UInt(24 bits) setAsReg() init 0

  val paramLastCnt = UInt(8 bits) setAsReg() init 0
  val inputCmdCnt = UInt(8 bits) setAsReg() init 0
  val outputCmdCnt = UInt(16 bits) setAsReg() init 0

  when(io.inputs.fire) {
    inputsCnt := inputsCnt + 1
  }
  when(io.auxInputs.fire) {
    auxInputsCnt := auxInputsCnt + 1
  }
  when(io.outputs.fire) {
    outputsCnt := outputsCnt + 1
  }
  when(io.params.fire) {
    paramsCnt := paramsCnt + 1
  }
  when(io.params.fire & io.params.last) {
    paramLastCnt := paramLastCnt + 1
  }
  when(io.inputsCmd.fire) {
    inputCmdCnt := inputCmdCnt + 1
  }
  when(io.outputsCmd.fire) {
    outputCmdCnt := outputCmdCnt + 1
  }

  configCtrl.readAndWrite(inputsCnt, 0x100, 0)
  configCtrl.readAndWrite(auxInputsCnt, 0x104, 0)
  configCtrl.readAndWrite(outputsCnt, 0x108, 0)
  configCtrl.readAndWrite(paramsCnt, 0x10C, 0)
  configCtrl.readAndWrite(paramLastCnt, 0x110, 0)
  configCtrl.readAndWrite(inputCmdCnt, 0x110, 8)
  configCtrl.readAndWrite(outputCmdCnt, 0x110, 16)
  configCtrl.read(firefly.coreStatus, 0x114, 0)

  val outCmdValid = RegNext(RegNext(io.outputsCmd.valid))
  val outCmdReady = RegNext(RegNext(io.outputsCmd.ready))
  val outValid = RegNext(RegNext(io.outputs.valid))
  val outReady = RegNext(RegNext(io.outputs.ready))

  configCtrl.read(outCmdValid, 0x120, 0)
  configCtrl.read(outCmdReady, 0x120, 8)
  configCtrl.read(outValid, 0x120, 16)
  configCtrl.read(outReady, 0x120, 24)

  firefly.configList.config0 := configWord0
  firefly.configList.config1 := configWord1
  firefly.configList.config2 := configWord2
  firefly.configList.config3 := configWord3
  firefly.configList.config4 := configWord4
  firefly.configList.config5 := configWord5
  firefly.configList.im2colConfig0 := im2colConfigWord0
  firefly.configList.im2colConfig1 := im2colConfigWord1
  firefly.configList.im2colConfig2 := im2colConfigWord2
  firefly.configList.im2colConfig3 := im2colConfigWord3
  firefly.configList.im2colConfig4 := im2colConfigWord4
  firefly.configList.im2colConfig5 := im2colConfigWord5
  firefly.configList.im2colConfig6 := im2colConfigWord6
  firefly.configList.im2colConfig7 := im2colConfigWord7
  firefly.configList.im2colConfig8 := im2colConfigWord8
}

object AXIFireFlyFaster extends App {

  SpinalConfig(
    nameWhenByFile = false,
    oneFilePerComponent = false,
    anonymSignalPrefix = "tmp",
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
  ).generateVerilog(new AXIFireFlyFaster(
    ClockDomain.external("ddrClk"),
    (16, 16, 4, 4),
    4096 * 4,
    "block",
    512,
    8,
    16,
    8,
    "if"
  ))
}
