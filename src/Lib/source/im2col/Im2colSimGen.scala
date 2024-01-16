package im2col

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import utils.RandomUInt

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Im2colSimGen(
                    channels: Int,
                    imgSize: (Int, Int),
                    padding: (Int, Int),
                    kernel: (Int, Int),
                    dilation: (Int, Int),
                    stride: (Int, Int),
                    parallelism: (Int, Int)
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

  val batch = channels / cp
  val merge = batch * stride._2
  val isCoalesceInt = merge % 2
  val isCoalesce = isCoalesceInt == 1
  val mergeCnt = padImgSize._2 / stride._2
  val mergeCoalesce = merge + (1 - isCoalesceInt)

  val pad4Conv_shape = List(batch, inImgSize._2, inImgSize._1)
  val pad4Conv_head = List(0, padding._2, padding._1)
  val pad4Conv_tail = List(0, padding._2 + additionalPad, padding._1)

  val pad4Coalesce_shape = List(merge)
  val pad4Coalesce_head = List(0)
  val pad4Coalesce_tail = List(1 - isCoalesceInt)

  val push_width = padImgSize._2 / stride._2 - 1
  val push_height = padImgSize._1 - 1
  val push_channels = mergeCoalesce - 1
  val push_heightReserve = dilatedKernel._1 - 1 + stride._1 - 1

  val pop_width = outImgSize._2 / pp - 1
  val pop_height = outImgSize._1 - 1
  val pop_channels = batch - 1
  val pop_kernelWidth = kernel._1 - 1
  val pop_kernelHeight = kernel._2 - 1
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
  val pop_offset = Range(0, pp).map(i => i * mergeCoalesce).toList

  val inImg = Array.ofDim[Int](channels, inImgSize._1, inImgSize._2)
  val padImg = Array.ofDim[Int](channels, padImgSize._1, padImgSize._2)
  val transImg = Array.ofDim[Int](channels, kernel._1, kernel._2, outImgSize._1, outImgSize._2)

  val bufferDepth = scala.math.pow(2, log2Up(mergeCoalesce * mergeCnt * dilatedKernel._1)).toInt


  def setConfig[T <: Data](dut: Im2col[T]) = {
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

    (dut.cfgPad4coalesce.shape, pad4Coalesce_shape).zipped.foreach(_ #= _)
    (dut.cfgPad4coalesce.padAtHead, pad4Coalesce_head).zipped.foreach(_ #= _)
    (dut.cfgPad4coalesce.padAtTail, pad4Coalesce_tail).zipped.foreach(_ #= _)

    println("pad4coalesce shape", pad4Coalesce_shape)
    println("pad4coalesce head", pad4Coalesce_head)
    println("pad4coalesce tail", pad4Coalesce_tail)

    dut.cfgPush.width #= push_width
    dut.cfgPush.height #= push_height
    dut.cfgPush.channels #= push_channels
    dut.cfgPush.heightReserve #= push_heightReserve

    println("push width", push_width)
    println("push height", push_height)
    println("push channels", push_channels)
    println("push heightReserve", push_heightReserve)

    dut.cfgPop.width #= pop_width
    dut.cfgPop.height #= pop_height
    dut.cfgPop.channels #= pop_channels
    dut.cfgPop.heightInc #= pop_heightInc

    println("pop width", pop_width)
    println("pop height", pop_height)
    println("pop channels", pop_channels)
    println("pop heightInc", pop_heightInc)

    dut.cfgPop.kernelWidth #= pop_kernelWidth
    dut.cfgPop.kernelHeight #= pop_kernelHeight

    println("pop kernelWidth", pop_kernelWidth)
    println("pop kernelHeight", pop_kernelHeight)

    dut.cfgPop.kernelWidthAddrInc #= pop_kernelWidthAddrInc
    dut.cfgPop.coalesceBound #= pop_coalesceBound
    dut.cfgPop.kernelHeightAddrInc #= pop_kernelHeightAddrInc
    dut.cfgPop.widthAddrInc #= pop_widthAddrInc
    dut.cfgPop.heightAddrInc #= pop_heightAddrInc
    (dut.cfgPop.offset, pop_offset).zipped.foreach(_ #= _)

    println("pop kernelWidthAddrInc", pop_kernelWidthAddrInc)
    println("pop coalesceBound", pop_coalesceBound)
    println("pop kernelHeightAddrInc", pop_kernelHeightAddrInc)
    println("pop widthAddrInc", pop_widthAddrInc)
    println("pop heightAddrInc", pop_heightAddrInc)
    println("pop offset", pop_offset)
  }

  def genImg() = {
    for (c <- 0 until channels) {
      for (h <- 0 until inImgSize._1) {
        for (w <- 0 until inImgSize._2) {
          inImg(c)(h)(w) = (c * h * w) % 16 //RandomUInt(4)
        }
      }
    }
  }

  def genPadImg() = {
    for (c <- 0 until channels) {
      for (h <- 0 until padImgSize._1) {
        for (w <- 0 until padImgSize._2) {
          padImg(c)(h)(w) = 0
        }
      }
      for (h <- 0 until inImgSize._1) {
        for (w <- 0 until inImgSize._2) {
          padImg(c)(h + padding._1)(w + padding._2) = inImg(c)(h)(w)
        }
      }
    }
  }

  def genTransImg() = {
    for (c <- 0 until channels) {
      for (h <- 0 until outImgSize._1) {
        for (w <- 0 until outImgSize._2) {
          val pin = (h * stride._1, w * stride._2)
          for (kh <- 0 until kernel._1) {
            for (kw <- 0 until kernel._2) {
              transImg(c)(kh)(kw)(h)(w) = padImg(c)(pin._1 + kh * dilation._1)(pin._2 + kw * dilation._2)
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
        for (c <- 0 until channels) {
          flatten.append(inImg(c)(h)(w))
        }
      }
    }
    flatten.toArray.grouped(cp).toArray
  }

  def genPadImgStream() = {
    val flatten = ArrayBuffer[Int]()
    for (h <- 0 until padImgSize._1) {
      for (w <- 0 until padImgSize._2) {
        for (c <- 0 until channels) {
          flatten.append(padImg(c)(h)(w))
        }
      }
    }
    flatten.toArray.grouped(cp).toArray
  }

  def genTransImgStream() = {
    val flatten = ArrayBuffer[Array[Array[Int]]]()
    for (h <- 0 until outImgSize._1) {
      for (wp <- 0 until outImgSize._2 / pp) {
        for (kh <- 0 until kernel._1) {
          for (kw <- 0 until kernel._2) {
            for (ch <- 0 until channels / cp) {
              val mat = Array.ofDim[Int](pp, cp)
              for (p <- 0 until pp) {
                for (c <- 0 until cp) {
                  mat(p)(c) = transImg(ch * cp + c)(kh)(kw)(h)(wp * pp + p)
                }
              }
              flatten.append(mat)
            }
          }
        }
      }
    }
    flatten.toArray
  }
}
