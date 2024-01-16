package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class StreamBits2StreamIndex(bitWidth: Int, indexWidth: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream Fragment(Bits(bitWidth bits))
    val output = master Stream UInt(indexWidth bits)
  }

  val cntWidth = indexWidth - log2Up(bitWidth)
  val cnt = UInt(cntWidth bits) setAsReg() init 0
  when(io.input.fire) {
    cnt := cnt + 1
    when(io.input.last) {
      cnt.clearAll()
    }
  }

  val linked = Stream(Linked(Bits(bitWidth bits), UInt(cntWidth bits)))

  linked.value := io.input.payload
  linked.linked := cnt
  linked.arbitrationFrom(io.input.throwWhen(!io.input.payload.orR))

  val queued = fifos.Queue(linked, 32)
  val localIndex = Stream(UInt(log2Up(bitWidth) bits))

  val maskStored = Bits(bitWidth bits) setAsReg() init 0
  val payload = queued.value
  val masked = payload & ~maskStored
  val lsb = OHMasking.firstV2(masked)
  val index = OHToUInt(lsb)
  val newMask = lsb | maskStored
  val maskUpdate = masked & ~lsb

  when(localIndex.fire)(maskStored := newMask)
  when(queued.fire)(maskStored.clearAll())

  queued.ready := localIndex.ready && !maskUpdate.orR
  localIndex.valid := queued.valid
  localIndex.payload := index

  io.output << localIndex.
    translateWith(index + (queued.linked << log2Up(bitWidth))).
    s2mPipe().m2sPipe()
}
