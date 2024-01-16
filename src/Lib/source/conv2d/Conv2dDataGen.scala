package conv2d

import spinal.core.log2Up

import scala.collection.mutable.ArrayBuffer

class Conv2dDataGen(
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
  val padImgSize = (
    inImgSize._1 + 2 * padding._1,
    if ((inImgSize._2 + 2 * padding._2) % stride._2 == 0)
      inImgSize._2 + 2 * padding._2 else
      inImgSize._2 + 2 * padding._2 + stride._2 - (inImgSize._2 + 2 * padding._2) % stride._2
  )
  val outImgSize = (
    (inImgSize._1 + 2 * padding._1 - dilatedKernel._1) / stride._1 + 1,
    (inImgSize._2 + 2 * padding._2 - dilatedKernel._2) / stride._2 + 1
  )

  require(channels % cp == 0)
  require(outImgSize._2 % pp == 0)
  require(padImgSize._2 % stride._2 == 0)

  val inImg = Array.ofDim[Int](channels, inImgSize._1, inImgSize._2)
  val padImg = Array.ofDim[Int](channels, padImgSize._1, padImgSize._2)
  val transImg = Array.ofDim[Int](channels, kernel._1, kernel._2, outImgSize._1, outImgSize._2)

  val batchChannel = channels / cp
  val widthInBuf = scala.math.ceil((padImgSize._2 * batchChannel).toFloat / (batchChannel * stride._2 * pp).toFloat).toInt * batchChannel * stride._2

  val bndOfLoop = Array(
    batchChannel - 1,
    kernel._2 - 1,
    kernel._1 - 1,
    outImgSize._2 / pp - 1,
    outImgSize._1 - 1
  )
  val incOfAddrLv1 = Array(
    1,
    0,
    dilation._1 * widthInBuf,
    stride._2 * batchChannel,
    stride._1 * widthInBuf
  )
  val incOfAddrLv2 = Array(
    batchChannel,
    stride._2 * batchChannel
  )

  val resHeight = padImgSize._1 - ((outImgSize._1 - 1) * stride._1 + dilatedKernel._1)
  val incOfnextLine = widthInBuf
  val incOfNextImg = (dilatedKernel._1 + resHeight) * widthInBuf

  val length = stride._2 - 1
  val skip = dilation._2

  val cacheSize = if (dilatedKernel._2 == 1) batchChannel else
    (dilatedKernel._1 - 1) * widthInBuf +
      batchChannel * scala.math.ceil((dilatedKernel._2 - stride._2).toFloat / pp.toFloat).toInt + 1

  val depth = scala.math.pow(2, log2Up(scala.math.max(cacheSize, stride._1 * widthInBuf)) + 1).toInt
  val cacheDummy = depth - (stride._2 * batchChannel) - 1

  val bndOfChannels = stride._2 * batchChannel - 1
  val bndOfWidth = padImgSize._2 / stride._2 - 1

  val padLeft = batchChannel * padding._2
  val padRight = (if ((inImgSize._2 + 2 * padding._2) % stride._2 == 0)
    padding._2 else padding._2 + stride._2 - (inImgSize._2 + 2 * padding._2) % stride._2) * batchChannel
  val padUpDown = batchChannel * padImgSize._2 * padding._1

  val inChannels = batchChannel - 1
  val inImgHeight = inImgSize._1 - 1
  val inImgWidth = inImgSize._2 - 1

  val padCtrl = Array(
    inChannels, inImgWidth, inImgHeight, padLeft, padUpDown, padRight, padUpDown
  )

  val pushCtrl = Array(
    bndOfChannels, bndOfWidth, incOfnextLine
  )

  val cacheCtrl = Array(
    cacheSize, cacheDummy
  )

  val convCtrl = Array(
    length, skip, incOfNextImg
  ) ++ bndOfLoop ++ incOfAddrLv1 ++ incOfAddrLv2

  def getConfigBits(bitWidth: Array[Array[Int]]) = {
    Array(
      utils.Array2BigInt(padCtrl, bitWidth(0)),
      utils.Array2BigInt(pushCtrl, bitWidth(1)),
      utils.Array2BigInt(cacheCtrl, bitWidth(2)),
      utils.Array2BigInt(convCtrl, bitWidth(3))
    )
  }

  def genImg() = {
    for (c <- 0 until channels) {
      for (h <- 0 until inImgSize._1) {
        for (w <- 0 until inImgSize._2) {
          inImg(c)(h)(w) = h * inImgSize._2 + w
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
