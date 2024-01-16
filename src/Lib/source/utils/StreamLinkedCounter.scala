package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object StreamLinkedCounter {
  def apply[T <: Data](source: Stream[Fragment[T]], width: Int) = {
    val ret = Stream(Fragment(Linked(source.fragmentType, UInt(width bits))))
    val cnt = Counter(width bits, inc = source.fire)
    when(cnt.willIncrement && source.last)(cnt.clear())
    ret.arbitrationFrom(source)
    ret.last := source.last
    ret.value := source.fragment
    ret.linked := cnt
    ret
  }
}
