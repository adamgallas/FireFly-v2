import spinal.core._
import spinal.lib._
import spinal.core.sim._
import utils.RandomUInt

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Im2colWithTSimGen(
                         timeStep: Int,
                         channels: Int,
                         imgSize: (Int, Int),
                         padding: (Int, Int),
                         kernel: (Int, Int),
                         dilation: (Int, Int),
                         stride: (Int, Int),
                         parallelism: (Int, Int),
                         reuse: Int = 1
                       ) {

  val inImgSize = imgSize
  val (pp, cp) = parallelism
  val dilatedKernel = (
    (kernel._1 - 1) * dilation._1 + 1,
    (kernel._2 - 1) * dilation._2 + 1
  )

  val idealPadWidth = inImgSize._2 + 2 * padding._2
  val stridePad = (stride._2 - idealPadWidth % stride._2) % stride._2
  val afterStridePadWidth = idealPadWidth + stridePad
  val idealOutWidth = (afterStridePadWidth - dilatedKernel._2) / stride._2 + 1
  val outWidthRes = (pp - idealOutWidth % pp) % pp
  val roundPad = outWidthRes * stride._2
  val additionalPad = roundPad + stridePad

  val padImgSize = (
    inImgSize._1 + 2 * padding._1,
    inImgSize._2 + 2 * padding._2 + additionalPad
  )
  val outImgSize = (
    (padImgSize._1 - dilatedKernel._1) / stride._1 + 1,
    (padImgSize._2 - dilatedKernel._2) / stride._2 + 1
  )

  require(channels % cp == 0)
  require(outImgSize._2 % pp == 0)
  require(padImgSize._2 % stride._2 == 0)

  val isStride1 = stride._2 == 1

  val batch = channels * timeStep / cp
  val merge = batch * stride._2
  val isCoalesceInt = merge % 2
  val isCoalesce = isCoalesceInt == 1
  val mergeCnt = padImgSize._2 / stride._2
  val mergeCoalesce = merge + (1 - isCoalesceInt)

  val pad4Conv_shape = List(batch, inImgSize._2, inImgSize._1)
  val pad4Conv_head = List(0, padding._2, padding._1)
  val pad4Conv_tail = List(0, padding._2 + additionalPad, padding._1)

  val pad4Coalesce_shape = List(merge - 1)
  val pad4Coalesce_head = List(0)
  val pad4Coalesce_tail = List(1 - isCoalesceInt)

  val push_width = padImgSize._2 / stride._2 - 1
  val push_height = padImgSize._1 - 1
  val push_channelTimeStep = merge - 1
  val push_heightReserve = dilatedKernel._1 - 1 + stride._1 - 1

  val pop_channels = channels / cp - 1
  val pop_timeStep = timeStep - 1
  val pop_width = outImgSize._2 / pp - 1
  val pop_height = outImgSize._1 - 1
  val pop_kernelWidth = kernel._2 - 1
  val pop_kernelHeight = kernel._1 - 1
  val pop_heightInc = stride._1

  val pop_kernelWidthAddrInc =
    if (isStride1) {
      if (isCoalesce) batch * dilation._2 else (batch + 1) * dilation._2
    } else {
      batch
    }
  val pop_coalesceBound =
    if (isStride1) {
      kernel._2
    } else {
      stride._2 - 1
    }
  val pop_kernelHeightAddrInc = mergeCoalesce * mergeCnt * dilation._1
  val pop_widthAddrInc = mergeCoalesce * pp
  val pop_heightAddrInc = mergeCoalesce * mergeCnt * stride._1
  val pop_timeStepAddrInc = channels / cp
  val pop_offset = Range(0, pp).map(i => i * mergeCoalesce).toList

  val inImg = Array.ofDim[Int](channels, timeStep, inImgSize._1, inImgSize._2)
  val padImg = Array.ofDim[Int](channels, timeStep, padImgSize._1, padImgSize._2)
  val transImg = Array.fill(channels)(Array.ofDim[Int](timeStep, kernel._1, kernel._2, outImgSize._1, outImgSize._2))

  val popReuse = reuse - 1
  val bufferDepth = scala.math.pow(2, log2Up(mergeCoalesce * mergeCnt * dilatedKernel._1)).toInt

  def setConfig[T <: Data](dut: Im2colWithT[T]) = {
    println("ideal pad size", (padImgSize._1, idealPadWidth))
    println("actual pad size", (padImgSize._1, padImgSize._2))
    println("ideal out width", (outImgSize._1, idealOutWidth))
    println("actual out width", outImgSize)

    (dut.cfgPad4conv.shape, pad4Conv_shape).zipped.foreach(_ #= _)
    (dut.cfgPad4conv.padAtHead, pad4Conv_head).zipped.foreach(_ #= _)
    (dut.cfgPad4conv.padAtTail, pad4Conv_tail).zipped.foreach(_ #= _)

    println("pad4conv shape", pad4Conv_shape)
    println("pad4conv head", pad4Conv_head)
    println("pad4conv tail", pad4Conv_tail)

    dut.cfgIm2col.coalesceShape #= pad4Coalesce_shape.head
    dut.cfgIm2col.coalescePad #= pad4Coalesce_tail.head

    println("pad4coalesce shape", pad4Coalesce_shape)
    println("pad4coalesce head", pad4Coalesce_head)
    println("pad4coalesce tail", pad4Coalesce_tail)

    dut.cfgIm2col.pushWidth #= push_width
    dut.cfgIm2col.pushHeight #= push_height
    dut.cfgIm2col.pushChannelsTimeStep #= push_channelTimeStep
    dut.cfgIm2col.heightReserve #= push_heightReserve

    println("push width", push_width)
    println("push height", push_height)
    println("push channels", push_channelTimeStep)
    println("push heightReserve", push_heightReserve)

    dut.cfgIm2col.popReuse #= popReuse
    dut.cfgIm2col.popWidth #= pop_width
    dut.cfgIm2col.popHeight #= pop_height
    dut.cfgIm2col.popChannels #= pop_channels
    dut.cfgIm2col.timeStep #= pop_timeStep
    dut.cfgIm2col.popKernelWidth #= pop_kernelWidth
    dut.cfgIm2col.popKernelHeight #= pop_kernelHeight

    println("pop width", pop_width)
    println("pop height", pop_height)
    println("pop channels", pop_channels)
    println("pop timeStep", pop_timeStep)
    println("pop kernelWidth", pop_kernelWidth)
    println("pop kernelHeight", pop_kernelHeight)
    println("pop reuse", popReuse)

    dut.cfgIm2col.kernelWidthAddrInc #= pop_kernelWidthAddrInc
    dut.cfgIm2col.kernelHeightAddrInc #= pop_kernelHeightAddrInc
    dut.cfgIm2col.widthAddrInc #= pop_widthAddrInc
    dut.cfgIm2col.heightAddrInc #= pop_heightAddrInc
    dut.cfgIm2col.timeStepAddrInc #= pop_timeStepAddrInc
    (dut.cfgIm2col.offset, pop_offset).zipped.foreach(_ #= _)

    println("pop kernelWidthAddrInc", pop_kernelWidthAddrInc)
    println("pop kernelHeightAddrInc", pop_kernelHeightAddrInc)
    println("pop widthAddrInc", pop_widthAddrInc)
    println("pop heightAddrInc", pop_heightAddrInc)
    println("pop timeStepAddrInc", pop_timeStepAddrInc)
    println("pop offset", pop_offset)

    dut.cfgIm2col.coalesceBound #= pop_coalesceBound
    dut.cfgIm2col.heightInc #= pop_heightInc

    println("pop coalesceBound", pop_coalesceBound)
    println("pop heightInc", pop_heightInc)
  }

  def genImg() = {
    for (c <- 0 until channels) {
      for (t <- 0 until timeStep) {
        for (h <- 0 until inImgSize._1) {
          for (w <- 0 until inImgSize._2) {
            inImg(c)(t)(h)(w) = (c * h * w) % 16
          }
        }
      }
    }
  }

  def genPadImg() = {
    for (c <- 0 until channels) {
      for (t <- 0 until timeStep) {
        for (h <- 0 until padImgSize._1) {
          for (w <- 0 until padImgSize._2) {
            padImg(c)(t)(h)(w) = 0
          }
        }
        for (h <- 0 until inImgSize._1) {
          for (w <- 0 until inImgSize._2) {
            padImg(c)(t)(h + padding._1)(w + padding._2) = inImg(c)(t)(h)(w)
          }
        }
      }
    }
  }

  def genTransImg() = {
    for (c <- 0 until channels) {
      for (t <- 0 until timeStep) {
        for (h <- 0 until outImgSize._1) {
          for (w <- 0 until outImgSize._2) {
            val pin = (h * stride._1, w * stride._2)
            for (kh <- 0 until kernel._1) {
              for (kw <- 0 until kernel._2) {
                transImg(c)(t)(kh)(kw)(h)(w) = padImg(c)(t)(pin._1 + kh * dilation._1)(pin._2 + kw * dilation._2)
              }
            }
          }
        }
      }
    }
  }

  def genInImgStream() = {
    val flatten = ArrayBuffer[Int]()
    for (h <- 0 until inImgSize._1) {
      for (w <- 0 until inImgSize._2) {
        for (t <- 0 until timeStep) {
          for (c <- 0 until channels) {
            flatten.append(inImg(c)(t)(h)(w))
          }
        }
      }
    }
    flatten.toArray.grouped(cp).toArray
  }

  def genPadImgStream() = {
    val flatten = ArrayBuffer[Int]()
    for (h <- 0 until padImgSize._1) {
      for (w <- 0 until padImgSize._2) {
        for (t <- 0 until timeStep) {
          for (c <- 0 until channels) {
            flatten.append(padImg(c)(t)(h)(w))
          }
        }
      }
    }
    flatten.toArray.grouped(cp).toArray
  }

  def genTransImgStream() = {
    val flatten = ArrayBuffer[Array[Array[Int]]]()
    for (h <- 0 until outImgSize._1) {
      for (wp <- 0 until outImgSize._2 / pp) {
        for (t <- 0 until timeStep) {
          for (kh <- 0 until kernel._1) {
            for (kw <- 0 until kernel._2) {
              for (ch <- 0 until channels / cp) {
                val mat = Array.ofDim[Int](pp, cp)
                for (p <- 0 until pp) {
                  for (c <- 0 until cp) {
                    mat(p)(c) = transImg(ch * cp + c)(t)(kh)(kw)(h)(wp * pp + p)
                  }
                }
                flatten.append(mat)
              }
            }
          }
        }
      }
    }
    flatten.toArray
  }
}
