package utils

import spinal.core._
import spinal.lib._

case class MemHistory[T1 <: Data, T2 <: Data](valueType: HardType[T1], linkedType: HardType[T2], depth: Int, tech: String = "auto") extends Component {
  val io = new Bundle {
    val source = slave(Stream(Fragment(linkedType)))
    val extract = master(Stream(Fragment(Linked(valueType, linkedType))))
    val update = slave(Stream(Fragment(valueType)))
  }
  noIoPrefix()

  val src = StreamLinkedCounter(io.source, log2Up(depth))
  val dst = StreamLinkedCounter(io.update, log2Up(depth))
  val mem = Mem(valueType, depth)
  if (tech != "auto")
    mem.addAttribute("ram_style", tech)

  val extractValid = RegInit(False)
  val extractData = mem.readSync(src.linked, src.ready)
  val extractLinked = RegNextWhen(src.value, src.ready)
  val extractLast = RegNextWhen(src.last, src.ready)
  when(io.extract.ready)(extractValid := Bool(false))
  when(src.ready)(extractValid := src.valid)
  src.ready := io.extract.isFree
  io.extract.valid := extractValid
  io.extract.value := extractData
  io.extract.linked := extractLinked
  io.extract.last := extractLast

  dst.freeRun()
  when(dst.valid) {
    mem.write(dst.linked, dst.value)
  }
}
