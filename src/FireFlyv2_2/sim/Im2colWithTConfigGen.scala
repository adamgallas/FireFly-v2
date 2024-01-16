class Im2colWithTConfigGen(
                            parallelism: (Int, Int, Int, Int),
                            timeStep: Int,
                            channels: (Int, Int),
                            imgSize: (Int, Int),
                            padding: (Int, Int),
                            kernel: (Int, Int),
                            dilation: (Int, Int),
                            stride: (Int, Int),
                            bufDepth: Int = 0
                          ) {
  val (m, v, n, t) = parallelism
  val (outChannel, inChannel) = channels
  val timeStepPass = timeStep / t

  val inImgSize = imgSize
  val (pp, cp) = (n, v)
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

  val inPixels = padImgSize._1 * padImgSize._2
  val inElems = inPixels * inChannel / v * timeStepPass
  val isReuse = inElems <= bufDepth

  val conv2dRealWidth = (inImgSize._2 + 2 * padding._2 - dilatedKernel._2) / stride._2 + 1
  val conv2dReadHeight = (inImgSize._1 + 2 * padding._1 - dilatedKernel._1) / stride._1 + 1

  val isStride1 = stride._2 == 1

  val batch = inChannel * timeStepPass / cp
  val merge = batch * stride._2
  val isCoalesceInt = merge % 2
  val isCoalesce = isCoalesceInt == 1
  val mergeCnt = padImgSize._2 / stride._2
  val mergeCoalesce = merge + (1 - isCoalesceInt)

  val pushChannelsTimeStep = merge - 1
  val pushWidth = padImgSize._2 / stride._2 - 1
  val pushHeight = padImgSize._1 - 1
  val pushHeightReserve = dilatedKernel._1 - 1 + stride._1 - 1

  val popReuse = if(isReuse) outChannel / m - 1 else 0
  val popChannels = inChannel / cp - 1
  val popTimeSteps = timeStepPass - 1
  val popWidth = outImgSize._2 / pp - 1
  val popHeight = outImgSize._1 - 1
  val popHeightInc = stride._1
  val popKernelWidth = kernel._2 - 1
  val popKernelHeight = kernel._1 - 1
  val popKernelWidthAddrInc =
    if (isStride1) {
      if (isCoalesce) batch * dilation._2 else (batch + 1) * dilation._2
    } else {
      batch
    }
  val popKernelHeightAddrInc = mergeCoalesce * mergeCnt * dilation._1
  val popWidthAddrInc = mergeCoalesce * pp
  val popHeightAddrInc = mergeCoalesce * mergeCnt * stride._1
  val popCoalesceBound =
    if (isStride1) {
      kernel._2
    } else {
      stride._2 - 1
    }
  val popTimeStepAddrInc = inChannel / cp
  val popOffset = mergeCoalesce

  val pad4ConvShape_0 = batch
  val pad4ConvShape_1 = inImgSize._2
  val pad4ConvShape_2 = inImgSize._1
  val pad4ConvHead_1 = padding._2
  val pad4ConvHead_2 = padding._1
  val pad4ConvTail_1 = padding._2 + additionalPad
  val pad4ConvTail_2 = padding._1
  val pad4CoalesceShape = merge - 1
  val pad4CoalesceTail = 1 - isCoalesceInt

  val weightReuse = timeStepPass * outImgSize._1 * outImgSize._2 / pp - 1
  val weightLength = kernel._1 * kernel._2 * inChannel / v - 1
  val inputPass = outChannel / m
  val outputLength = timeStepPass * outImgSize._1 * outImgSize._2 * outChannel / m
  val singlePassLength = timeStepPass * outImgSize._1 * outImgSize._2 - 1
}
