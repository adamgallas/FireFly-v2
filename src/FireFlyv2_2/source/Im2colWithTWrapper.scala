import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Im2colWithTWrapper[T <: Data](dataType: HardType[T], cp: Int, pp: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val io = new Bundle {
    val push = slave(Stream(Vec(dataType, cp)))
    val pop = master(Stream(Vec(Vec(dataType, cp), pp)))
  }

  val im2col = new Im2colWithT(dataType, cp, pp, bufferDepth, tech)
  io.push >> im2col.io.push
  io.pop << im2col.io.pop

  val config = new Bundle {
    val popReuse = in UInt(im2col.cfgIm2col.popReuse.getBitsWidth bits)
    val pushChannelsTimeStep = in UInt (im2col.cfgIm2col.pushChannelsTimeStep.getBitsWidth bits)
    val pushWidth = in UInt (im2col.cfgIm2col.pushWidth.getBitsWidth bits)
    val pushHeight = in UInt (im2col.cfgIm2col.pushHeight.getBitsWidth bits)
    val pushHeightReserve = in UInt (im2col.cfgIm2col.heightReserve.getBitsWidth bits)

    val timeStep = in UInt (im2col.cfgIm2col.timeStep.getBitsWidth bits)
    val popChannels = in UInt (im2col.cfgIm2col.popChannels.getBitsWidth bits)
    val popWidth = in UInt (im2col.cfgIm2col.popWidth.getBitsWidth bits)
    val popHeight = in UInt (im2col.cfgIm2col.popHeight.getBitsWidth bits)
    val popHeightInc = in UInt (im2col.cfgIm2col.heightInc.getBitsWidth bits)
    val popKernelWidth = in UInt (im2col.cfgIm2col.popKernelWidth.getBitsWidth bits)
    val popKernelHeight = in UInt (im2col.cfgIm2col.popKernelHeight.getBitsWidth bits)
    val popKernelWidthAddrInc = in UInt (im2col.cfgIm2col.kernelWidthAddrInc.getBitsWidth bits)
    val popKernelHeightAddrInc = in UInt (im2col.cfgIm2col.kernelHeightAddrInc.getBitsWidth bits)
    val popTimeStepInc = in UInt (im2col.cfgIm2col.timeStepAddrInc.getBitsWidth bits)
    val popWidthAddrInc = in UInt (im2col.cfgIm2col.widthAddrInc.getBitsWidth bits)
    val popHeightAddrInc = in UInt (im2col.cfgIm2col.heightAddrInc.getBitsWidth bits)
    val popCoalesceBound = in UInt (im2col.cfgIm2col.coalesceBound.getBitsWidth bits)
    val popOffset = in UInt (im2col.cfgIm2col.offset.head.getBitsWidth bits)

    val pad4ConvShape_0 = in UInt (im2col.cfgPad4conv.shape.head.getBitsWidth bits)
    val pad4ConvShape_1 = in UInt (im2col.cfgPad4conv.shape(1).getBitsWidth bits)
    val pad4ConvShape_2 = in UInt (im2col.cfgPad4conv.shape(2).getBitsWidth bits)
    val pad4ConvHead_1 = in UInt (4 bits)
    val pad4ConvHead_2 = in UInt (4 bits)
    val pad4ConvTail_1 = in UInt (4 bits)
    val pad4ConvTail_2 = in UInt (4 bits)
    val pad4CoalesceShape = in UInt (im2col.cfgIm2col.coalesceShape.getBitsWidth bits)
    val pad4CoalesceTail = in UInt (1 bits)
  }

  im2col.cfgIm2col.popReuse := RegNext(config.popReuse)
  im2col.cfgIm2col.pushChannelsTimeStep := RegNext(config.pushChannelsTimeStep)
  im2col.cfgIm2col.pushWidth := RegNext(config.pushWidth)
  im2col.cfgIm2col.pushHeight := RegNext(config.pushHeight)
  im2col.cfgIm2col.heightReserve := RegNext(config.pushHeightReserve)

  im2col.cfgIm2col.timeStep := RegNext(config.timeStep)
  im2col.cfgIm2col.popChannels := RegNext(config.popChannels)
  im2col.cfgIm2col.popWidth := RegNext(config.popWidth)
  im2col.cfgIm2col.popHeight := RegNext(config.popHeight)
  im2col.cfgIm2col.heightInc := RegNext(config.popHeightInc)
  im2col.cfgIm2col.popKernelWidth := RegNext(config.popKernelWidth)
  im2col.cfgIm2col.popKernelHeight := RegNext(config.popKernelHeight)
  im2col.cfgIm2col.kernelWidthAddrInc := RegNext(config.popKernelWidthAddrInc)
  im2col.cfgIm2col.kernelHeightAddrInc := RegNext(config.popKernelHeightAddrInc)
  im2col.cfgIm2col.timeStepAddrInc := RegNext(config.popTimeStepInc)
  im2col.cfgIm2col.widthAddrInc := RegNext(config.popWidthAddrInc)
  im2col.cfgIm2col.heightAddrInc := RegNext(config.popHeightAddrInc)
  im2col.cfgIm2col.coalesceBound := RegNext(config.popCoalesceBound)
  im2col.cfgIm2col.offset := RegNext(utils.ConstRangeVecGen(config.popOffset, pp - 1, im2col.cfgIm2col.offset.head.getBitsWidth))
  im2col.cfgIm2col.coalesceShape := RegNext(config.pad4CoalesceShape)
  im2col.cfgIm2col.coalescePad := RegNext(config.pad4CoalesceTail).resized

  im2col.cfgPad4conv.shape.head := RegNext(config.pad4ConvShape_0)
  im2col.cfgPad4conv.shape(1) := RegNext(config.pad4ConvShape_1)
  im2col.cfgPad4conv.shape(2) := RegNext(config.pad4ConvShape_2)
  im2col.cfgPad4conv.padAtHead.head := 0
  im2col.cfgPad4conv.padAtHead(1) := RegNext(config.pad4ConvHead_1).resize(im2col.cfgPad4conv.padAtHead(1).getBitsWidth)
  im2col.cfgPad4conv.padAtHead(2) := RegNext(config.pad4ConvHead_2).resize(im2col.cfgPad4conv.padAtHead(2).getBitsWidth)
  im2col.cfgPad4conv.padAtTail.head := 0
  im2col.cfgPad4conv.padAtTail(1) := RegNext(config.pad4ConvTail_1).resize(im2col.cfgPad4conv.padAtTail(1).getBitsWidth)
  im2col.cfgPad4conv.padAtTail(2) := RegNext(config.pad4ConvTail_2).resize(im2col.cfgPad4conv.padAtTail(2).getBitsWidth)

//  val expose = im2col.expose.toIo()
}

object Im2colWithTWrapper {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2colWithTWrapper(Bits(4 bits), 16, 8, 8192 * 8, "ultra"))
  }
}
