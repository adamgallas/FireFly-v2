package im2col

import fifos.StreamFifoHighPerf
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Im2colWrapper[T <: Data](dataType: HardType[T], cp: Int, pp: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val io = new Bundle {
    val push = slave(Stream(Vec(dataType, cp)))
    val pop = master(Stream(Vec(Vec(dataType, cp), pp)))
  }

  val im2col = new Im2col(dataType, cp, pp, bufferDepth, tech)
  io.push >> im2col.io.push
  io.pop << im2col.io.pop

  val config = new Bundle {
    val pushChannels = in UInt (im2col.cfgPush.channels.getBitsWidth bits)
    val pushWidth = in UInt (im2col.cfgPush.width.getBitsWidth bits)
    val pushHeight = in UInt (im2col.cfgPush.height.getBitsWidth bits)
    val pushHeightReserve = in UInt (im2col.cfgPush.heightReserve.getBitsWidth bits)

    val popChannels = in UInt (im2col.cfgPop.channels.getBitsWidth bits)
    val popWidth = in UInt (im2col.cfgPop.width.getBitsWidth bits)
    val popHeight = in UInt (im2col.cfgPop.height.getBitsWidth bits)
    val popHeightInc = in UInt (im2col.cfgPop.heightInc.getBitsWidth bits)
    val popKernelWidth = in UInt (im2col.cfgPop.kernelWidth.getBitsWidth bits)
    val popKernelHeight = in UInt (im2col.cfgPop.kernelHeight.getBitsWidth bits)
    val popKernelWidthAddrInc = in UInt (im2col.cfgPop.kernelWidthAddrInc.getBitsWidth bits)
    val popKernelHeightAddrInc = in UInt (im2col.cfgPop.kernelHeightAddrInc.getBitsWidth bits)
    val popWidthAddrInc = in UInt (im2col.cfgPop.widthAddrInc.getBitsWidth bits)
    val popHeightAddrInc = in UInt (im2col.cfgPop.heightAddrInc.getBitsWidth bits)
    val popCoalesceBound = in UInt (im2col.cfgPop.coalesceBound.getBitsWidth bits)
    val popOffset = in UInt (im2col.cfgPop.offset.head.getBitsWidth bits)

    val pad4ConvShape_0 = in UInt (im2col.cfgPad4conv.shape.head.getBitsWidth bits)
    val pad4ConvShape_1 = in UInt (im2col.cfgPad4conv.shape(1).getBitsWidth bits)
    val pad4ConvShape_2 = in UInt (im2col.cfgPad4conv.shape(2).getBitsWidth bits)
    val pad4ConvHead_1 = in UInt (4 bits)
    val pad4ConvHead_2 = in UInt (4 bits)
    val pad4ConvTail_1 = in UInt (4 bits)
    val pad4ConvTail_2 = in UInt (4 bits)
    val pad4CoalesceShape = in UInt (im2col.cfgPad4coalesce.shape.head.getBitsWidth bits)
    val pad4CoalesceTail = in UInt (1 bits)
  }

  im2col.cfgPush.channels := RegNext(config.pushChannels)
  im2col.cfgPush.width := RegNext(config.pushWidth)
  im2col.cfgPush.height := RegNext(config.pushHeight)
  im2col.cfgPush.heightReserve := RegNext(config.pushHeightReserve)

  im2col.cfgPop.channels := RegNext(config.popChannels)
  im2col.cfgPop.width := RegNext(config.popWidth)
  im2col.cfgPop.height := RegNext(config.popHeight)
  im2col.cfgPop.heightInc := RegNext(config.popHeightInc)
  im2col.cfgPop.kernelWidth := RegNext(config.popKernelWidth)
  im2col.cfgPop.kernelHeight := RegNext(config.popKernelHeight)
  im2col.cfgPop.kernelWidthAddrInc := RegNext(config.popKernelWidthAddrInc)
  im2col.cfgPop.kernelHeightAddrInc := RegNext(config.popKernelHeightAddrInc)
  im2col.cfgPop.widthAddrInc := RegNext(config.popWidthAddrInc)
  im2col.cfgPop.heightAddrInc := RegNext(config.popHeightAddrInc)
  im2col.cfgPop.coalesceBound := RegNext(config.popCoalesceBound)
  im2col.cfgPop.offset := RegNext(utils.ConstRangeVecGen(config.popOffset, pp - 1, im2col.cfgPop.offset.head.getBitsWidth))

  im2col.cfgPad4conv.shape.head := RegNext(config.pad4ConvShape_0)
  im2col.cfgPad4conv.shape(1) := RegNext(config.pad4ConvShape_1)
  im2col.cfgPad4conv.shape(2) := RegNext(config.pad4ConvShape_2)
  im2col.cfgPad4conv.padAtHead.head := 0
  im2col.cfgPad4conv.padAtHead(1) := RegNext(config.pad4ConvHead_1).resize(im2col.cfgPad4conv.padAtHead(1).getBitsWidth)
  im2col.cfgPad4conv.padAtHead(2) := RegNext(config.pad4ConvHead_2).resize(im2col.cfgPad4conv.padAtHead(2).getBitsWidth)
  im2col.cfgPad4conv.padAtTail.head := 0
  im2col.cfgPad4conv.padAtTail(1) := RegNext(config.pad4ConvTail_1).resize(im2col.cfgPad4conv.padAtTail(1).getBitsWidth)
  im2col.cfgPad4conv.padAtTail(2) := RegNext(config.pad4ConvTail_2).resize(im2col.cfgPad4conv.padAtTail(2).getBitsWidth)
  im2col.cfgPad4coalesce.shape.head := RegNext(config.pad4CoalesceShape)
  im2col.cfgPad4coalesce.padAtHead.head := 0
  im2col.cfgPad4coalesce.padAtTail.head := RegNext(config.pad4CoalesceTail).resized
}

object Im2colWrapper {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2colWrapper(Bits(4 bits), 16, 8, 8192, "ultra"))
  }
}