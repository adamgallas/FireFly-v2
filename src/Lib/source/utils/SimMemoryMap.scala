package utils

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable

class SimMemoryMap(
                    memSize: BigInt,
                    cmdQueueLimit: Int,
                    pendingCnt: Int,
                    mm2sDataValidRand: Boolean = false,
                    s2mmDataReadyRand: Boolean = false
                  ) {

  val mm2sCmdQueue = mutable.Queue[(Int, Int)]()
  val mm2sCmdPendingQueue = mutable.Queue[(Int, Int)]()

  val s2mmCmdQueue = mutable.Queue[(Int, Int)]()
  val s2mmCmdPendingQueue = mutable.Queue[(Int, Int)]()

  val queueLimit = cmdQueueLimit
  val mem = Array.fill(memSize.toInt)(0.toByte)

  private def Bits2AddrLen(cmd: Bits) = {
    val data = cmd.toBigInt
    val addr = (data >> 32) & 0x7FFFFFFF
    val len = data & 0x7FFFFF
    (addr.toInt, len.toInt)
  }

  private def s2mmCmdQNotFull(limit: Int): Boolean = s2mmCmdQueue.length < limit

  private def s2mmCmdEnqueue(cmd: (Int, Int)): Unit = s2mmCmdQueue.enqueue(cmd)

  private def mm2sCmdQNotFull(limit: Int): Boolean = mm2sCmdQueue.length < limit

  private def mm2sCmdEnqueue(cmd: (Int, Int)): Unit = mm2sCmdQueue.enqueue(cmd)

  private def cmd2DataArray(cmd: (Int, Int), busBytes: Int) = {
    val (addr, len) = cmd
    mem.slice(addr, addr + len).grouped(busBytes).map(_.reverse).map(
      _.foldLeft(BigInt(0))((a, b) => (a << 8) | (b & 0xff))
    ).toArray
  }

  def mm2sCmdSimThread(stream: Stream[Bits], clockDomain: ClockDomain) = {
    val ret = fork {
      SimStream2Queue(stream, clockDomain, Bits2AddrLen, queueLimit, mm2sCmdQNotFull, mm2sCmdEnqueue)
    }
    ret
  }

  def s2mmCmdSimThread(stream: Stream[Bits], clockDomain: ClockDomain) = {
    val ret = fork {
      SimStream2Queue(stream, clockDomain, Bits2AddrLen, queueLimit, s2mmCmdQNotFull, s2mmCmdEnqueue)
    }
    ret
  }

  def s2mmPendingSimThread(clockDomain: ClockDomain) = {
    val ret = fork {
      while (true) {
        clockDomain.waitSampling()
        if (s2mmCmdQueue.nonEmpty) {
          clockDomain.waitSampling(pendingCnt)
          s2mmCmdPendingQueue.enqueue(s2mmCmdQueue.dequeue())
        }
      }
    }
    ret
  }

  def mm2sPendingSimThread(clockDomain: ClockDomain) = {
    val ret = fork {
      while (true) {
        clockDomain.waitSampling()
        if (mm2sCmdQueue.nonEmpty) {
          clockDomain.waitSampling(pendingCnt)
          mm2sCmdPendingQueue.enqueue(mm2sCmdQueue.dequeue())
        }
      }
    }
    ret
  }

  def mm2sDataSimThread[T <: Data](
                                    stream: Stream[T],
                                    clockDomain: ClockDomain,
                                    driver: (T, BigInt) => Unit
                                  ) = {

    val bytes = stream.payload.getBitsWidth / 8
    val ret = fork {
      stream.valid #= false
      while (true) {
        clockDomain.waitSampling()
        if (mm2sCmdPendingQueue.nonEmpty) {
          val (addr, len) = mm2sCmdPendingQueue.dequeue()
          val dataArray = cmd2DataArray((addr, len), bytes)
          SimArray2Stream(stream, dataArray, clockDomain, driver, alwaysValid = !mm2sDataValidRand)
        }
        stream.valid #= false
      }
    }
    ret
  }

  def mm2sFragmentDataSimThread[T <: Data](
                                            stream: Stream[Fragment[T]],
                                            clockDomain: ClockDomain,
                                            driver: (T, BigInt) => Unit
                                          ) = {

    val bytes = stream.payload.getBitsWidth / 8
    val ret = fork {
      stream.valid #= false
      stream.last #= false
      while (true) {
        clockDomain.waitSampling()
        if (mm2sCmdPendingQueue.nonEmpty) {
          val (addr, len) = mm2sCmdPendingQueue.dequeue()
          val dataArray = cmd2DataArray((addr, len), bytes)
          SimArray2StreamFragment(stream, dataArray, clockDomain, driver, alwaysValid = !mm2sDataValidRand)
        }
        stream.valid #= false
        stream.last #= false
      }
    }
    ret
  }

  def s2mmDataSimThread[T <: Data](
                                    stream: Stream[T],
                                    clockDomain: ClockDomain,
                                    driver: T => BigInt
                                  ) = {

    val bytes = stream.payload.getBitsWidth / 8
    val ret = fork {
      stream.ready #= false
      while (true) {
        clockDomain.waitSampling()
        if (s2mmCmdPendingQueue.nonEmpty) {
          val (addr, len) = s2mmCmdPendingQueue.dequeue()
          val dataArray = SimStream2Array(stream, len / bytes, clockDomain, driver, alwaysReady = !s2mmDataReadyRand).toArray
          val byteDataArray = dataArray.flatMap(x => BigInt2ByteArray(x, bytes))
          Array.copy(byteDataArray, 0, mem, addr, len)
        }
        stream.ready #= false
      }
    }
    ret
  }
}
