package im2col

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Im2col3x3[T <: Data](dataType: HardType[T], cp: Int, bufferDepth: Int, tech: String = "block") extends Component {
  val addrWidth = log2Up(bufferDepth)
  val io = new Bundle {
    val push = slave(Stream(Vec(dataType, cp)))
    val pop = master(Stream(Vec(dataType, cp)))
  }

  val config = new Bundle {
    val width = in UInt (10 bits)
    val height = in UInt (10 bits)
    val channels = in UInt (10 bits)
    val heightInc = in UInt (addrWidth bits)
  }

  val im2col = new Im2col(dataType, cp, 1, bufferDepth, tech)

  val channels_1 = config.channels - 1

  io.push >> im2col.io.push
  io.pop.arbitrationFrom(im2col.io.pop)
  io.pop.payload := im2col.io.pop.payload(0)

  im2col.cfgPad4conv.shape := Vec(config.channels.resized, config.width, config.height)
  im2col.cfgPad4conv.padAtHead := Vec(U(0), U(1), U(1))
  im2col.cfgPad4conv.padAtTail := Vec(U(0), U(1), U(1))

  im2col.cfgPad4coalesce.shape := Vec(config.channels.resized)
  im2col.cfgPad4coalesce.padAtHead := Vec(U(0))
  im2col.cfgPad4coalesce.padAtTail := Vec(U(0))

  im2col.cfgPush.channels := channels_1.resized
  im2col.cfgPush.width := config.width + 1
  im2col.cfgPush.height := config.height + 1
  im2col.cfgPush.heightReserve := U(2)

  im2col.cfgPop.channels := channels_1.resized
  im2col.cfgPop.width := config.width - 1
  im2col.cfgPop.height := config.height - 1
  im2col.cfgPop.heightInc := U(1)

  im2col.cfgPop.kernelWidth := U(2)
  im2col.cfgPop.kernelHeight := U(2)
  im2col.cfgPop.kernelWidthAddrInc := config.channels.resized
  im2col.cfgPop.coalesceBound := U(3)
  im2col.cfgPop.kernelHeightAddrInc := config.heightInc
  im2col.cfgPop.widthAddrInc := config.channels.resized
  im2col.cfgPop.heightAddrInc := config.heightInc
  im2col.cfgPop.offset(0).clearAll()
}

object Im2col3x3 {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Im2col3x3(UInt(8 bits), 4, 1024))
  }
}
