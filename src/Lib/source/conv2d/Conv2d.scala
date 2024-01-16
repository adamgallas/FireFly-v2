package conv2d

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Conv2d[T <: Data](dataType: HardType[T], pp: Int, depth: Int) extends Component {
  require(isPow2(depth))
  val width = log2Up(depth)
  val boundBitWidth = List(10, 4, 4, 10, 10)
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(Vec(dataType, pp)))
  }

  val config = new Bundle {
    val padCtrl = new Bundle {
      val channels = in UInt (boundBitWidth.head bits)
      val imgWidth = in UInt (boundBitWidth.takeRight(2).head bits)
      val imgHeight = in UInt (boundBitWidth.takeRight(2).last bits)
      val imgLeftPad = in UInt (16 bits)
      val imgUpPad = in UInt (16 bits)
      val imgRightPad = in UInt (16 bits)
      val imgDownPad = in UInt (16 bits)
    }

    val pushCtrl = new Bundle {
      val bndOfChannels = in UInt (10 bits)
      val bndOfWidth = in UInt (10 bits)
      val incOfnextLine = in UInt (width bits)
    }

    val cacheCtrl = new Bundle {
      val cacheSize = in UInt (width bits)
      val cacheDummy = in UInt (width bits)
    }

    val convCtrl = new Bundle {
      val length = in UInt (2 bits)
      val dilation = in UInt (2 bits)
      val incOfNextImg = in UInt (width bits)
      val bndOfLoop = in Vec boundBitWidth.map(w => UInt(w bits))
      val incOfAddrLv1 = in Vec(UInt(width bits), boundBitWidth.length)
      val incOfAddrLv2 = in Vec(UInt(width bits), 2)
    }

    val ctrlBitWidth = Array(
      padCtrl.elements.map(_._2.getBitsWidth).toArray,
      pushCtrl.elements.map(_._2.getBitsWidth).toArray,
      cacheCtrl.elements.map(_._2.getBitsWidth).toArray,
      Array(2, 2, width) ++
        convCtrl.bndOfLoop.map(_.getWidth) ++
        convCtrl.incOfAddrLv1.map(_.getWidth) ++
        convCtrl.incOfAddrLv2.map(_.getWidth)
    )
  }
  println(config.padCtrl.getBitsWidth)
  println(config.pushCtrl.getBitsWidth)
  println(config.cacheCtrl.getBitsWidth)
  println(config.convCtrl.getBitsWidth)

  val padLogic = new Area {

    import config.padCtrl._

    val signal = new Bundle {
      val push = Stream(dataType)
      val pop = Stream(dataType)
    }

    val inImgColCnt = UInt(imgWidth.getWidth bits) setAsReg() init 0
    val inImgChCnt = UInt(channels.getWidth bits) setAsReg() init 0
    val inImgColOvf = inImgColCnt === imgWidth
    val inImgChOvf = inImgChCnt === channels

    when(signal.push.fire) {
      inImgChCnt := inImgChCnt + 1
      when(inImgChOvf) {
        inImgChCnt.clearAll()
        inImgColCnt := inImgColCnt + 1
        when(inImgColOvf) {
          inImgColCnt.clearAll()
        }
      }
    }

    val rowsWithLast = signal.push.addFragmentLast(inImgColOvf && inImgChOvf)
    val leftPadded = utils.StreamAddDummyHeader(rowsWithLast, imgLeftPad)
    val rightPadded = utils.StreamAddDummyTail(leftPadded, imgRightPad)

    val rowCnt = UInt(imgHeight.getWidth bits) setAsReg() init 0
    val rowCntOvf = rowCnt === imgHeight
    when(rightPadded.fire && rightPadded.last) {
      rowCnt := rowCnt + 1
      when(rowCntOvf) {
        rowCnt.clearAll()
      }
    }

    val leftRightPadded = utils.StreamDropLast(rightPadded).addFragmentLast(rowCntOvf && rightPadded.last)
    val upPadded = utils.StreamAddDummyHeader(leftRightPadded, imgUpPad)
    val downPadded = utils.StreamAddDummyTail(upPadded, imgDownPad)
    signal.pop << utils.StreamDropLast(downPadded)
  }

  val pushLogic = new Area {

    import config.pushCtrl._

    val signal = new Bundle {
      val push = Stream(dataType)
      val pushCmd = Vec(Flow(UInt(width bits)), pp)
      val full = Bool()
    }

    val pushPtr = Vec(UInt(width bits), pp)
    val pushPtrBase = UInt(width bits) setAsReg() init 0
    val pushPrtBaseNext = pushPtrBase + incOfnextLine

    val select = UInt(log2Up(pp) bits) setAsReg() init 0
    val (cnt, cntOvf) = utils.LoopCounter(List(bndOfChannels, bndOfWidth), signal.push.fire)
    val branch = StreamDemux(signal.push, select, pp)
    branch.foreach(_.ready := !signal.full)

    for (i <- 0 until pp) {
      pushPtr(i).setAsReg().init(0)
      when(branch(i).fire) {
        pushPtr(i) := pushPtr(i) + 1
      }
    }
    when(signal.push.fire && cntOvf.head) {
      if (select.getWidth != 0) select := select + 1
      when(cntOvf(1)) {
        pushPtrBase := pushPrtBaseNext
        pushPtr.foreach(_ := pushPrtBaseNext)
        select.clearAll()
      }
    }
    (signal.pushCmd, branch).zipped.foreach(_.valid := _.fire)
    (signal.pushCmd, pushPtr).zipped.foreach(_.payload := _)
  }

  val popLogic = new Area {
    val signal = new Bundle {
      val popCmd = Vec(Flow(UInt(width bits)), pp)
      val empty = Bool()

      val update = Bool()
      val address = Vec(UInt(width bits), pp)
      val select = Vec(UInt(log2Up(pp) bits), pp)
      val data = Vec(dataType, pp)
    }

    val oh = Vec(signal.select.map(s => UIntToOh(s).as(Vec(Bool(), pp)))).transpose
    val dec = RegNext(Vec(oh.map(s => OHToUInt(Vec(s).asBits))))
    val enc = Delay(signal.select, 4)

    val addr = RegNext(signal.address)
    val addrDec = RegNext(Vec(dec.map(s => addr(s))))
    val dataDly = RegNext(signal.data)
    val resort = RegNext(Vec(enc.map(sel => dataDly(sel))))

    signal.popCmd.foreach(_.valid.set())
    (signal.popCmd, addrDec).zipped.foreach(_.payload := _)

    val pop = Event
    pop.valid := !signal.empty
    pop.ready := io.pop.ready
    signal.update := pop.fire

    val fifo = fifos.StreamFifoHighPerf(Vec(dataType, pp), 8)
    fifo.io.push.valid := Delay(pop.fire, 5, init = False)
    fifo.io.push.payload := resort
    fifo.io.pop >> io.pop
  }

  val cacheControl = new Area {

    import config.cacheCtrl._

    val signal = new Bundle {
      val empty = Bool()
      val full = Bool()
      val cacheStart = UInt(width bits)
    }

    val interval = (pushLogic.pushPtr.last.expand + depth - signal.cacheStart.expand).dropHigh(1).asUInt
    signal.empty := interval < cacheSize
    signal.full := interval > cacheDummy
  }

  val conv2d = new Area {

    import config.convCtrl._

    val signal = new Bundle {
      val update = Bool()
      val cacheStart = UInt(width bits)
      val address = Vec(UInt(width bits), pp)
      val select = Vec(UInt(log2Up(pp) bits), pp)
    }

    val (cnt, cntOvf) = utils.LoopCounter(bndOfLoop.toList, signal.update)
    val addrLv1, addrLv1Next = Vec(UInt(width bits), boundBitWidth.length)
    addrLv1.foreach(_.setAsReg().init(0))
    (addrLv1, addrLv1Next).zipped.foreach(_ := _)

    (addrLv1Next, addrLv1).zipped.foreach(_ := _)
    when(signal.update)(addrLv1Next.head := addrLv1.head + incOfAddrLv1.head)
    for (i <- 1 until boundBitWidth.length) {
      when(signal.update && cntOvf.take(i).reduceLeft(_ && _)) {
        addrLv1Next(i - 1) := addrLv1Next(i)
        addrLv1Next(i) := addrLv1(i) + incOfAddrLv1(i)
      }
    }
    when(signal.update && cntOvf.reduceLeft(_ && _))(addrLv1Next.last := addrLv1.last + incOfNextImg)

    val lenCnt = UInt(2 bits) setAsReg() init 0
    val lenCntOvf = lenCnt === length
    when(signal.update && cntOvf.head) {
      lenCnt := lenCnt + 1
      when(lenCntOvf || cntOvf(1))(lenCnt.clearAll())
    }

    val bank = for (i <- 0 until pp) yield new Area {
      val sel = UInt(log2Up(pp) bits) setAsReg() init i
      val selNext = sel.expand + dilation.resize(log2Up(pp) + 1)
      val selOvf = selNext.takeHigh(1).asBool === True
      when(signal.update && cntOvf.head) {
        when(lenCntOvf)(sel := selNext.resized)
        when(cntOvf(1))(sel := i)
      }

      val addrLv2, addrLv2Base = UInt(width bits) setAsReg() init 0
      val addrLv2Next, addrLv2BaseNext = UInt(width bits)
      addrLv2Base := addrLv2BaseNext
      addrLv2 := addrLv2Next

      addrLv2Next := addrLv2
      addrLv2BaseNext := addrLv2Base
      when(signal.update && cntOvf.head) {
        addrLv2Next := addrLv2 + incOfAddrLv2(0)
        when(lenCntOvf) {
          addrLv2Next := addrLv2Base
          when(selOvf) {
            addrLv2Next := addrLv2BaseNext
            addrLv2BaseNext := addrLv2Base + incOfAddrLv2(1)
          }
        }
        when(cntOvf(1)) {
          addrLv2Next.clearAll()
          addrLv2BaseNext.clearAll()
        }
      }
    }
    signal.select := Vec(bank.map(_.sel))
    signal.address := Vec(bank.map(_.addrLv2 + addrLv1.head))
    signal.cacheStart := addrLv1(3)
  }

  io.push >> padLogic.signal.push
  padLogic.signal.pop >> pushLogic.signal.push

  pushLogic.signal.full := cacheControl.signal.full
  popLogic.signal.empty := cacheControl.signal.empty
  cacheControl.signal.cacheStart := conv2d.signal.cacheStart

  popLogic.signal.address := conv2d.signal.address
  popLogic.signal.select := conv2d.signal.select
  conv2d.signal.update := popLogic.signal.update

  val ram = Array.fill(pp)(Mem(dataType, depth))
  (ram, pushLogic.signal.pushCmd).zipped.foreach((r, c) => r.write(c.payload, pushLogic.signal.push.payload, c.valid))
  popLogic.signal.data := Vec((ram, popLogic.signal.popCmd).zipped.map((r, c) => r.readSync(c.payload)))
}
