package utils

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class FixedLenStridedAddrCmdGen(
                                      widthOfInc: List[Int],
                                      widthOfBnd: List[Int],
                                      addrWidth: Int,
                                      lenWidth: Int
                                    ) extends Component {
  val io = new Bundle {
    val startPulse = in Bool()
    val busy = out Bool()
    val fixLen = in(UInt(lenWidth bits))
    val inc = in(Vec(widthOfInc.map(w => UInt(w bits))))
    val bound = in(Vec(widthOfBnd.map(w => UInt(w bits))))
    val cmd = master(Stream(utils.Linked(UInt(addrWidth bits), UInt(lenWidth bits))))
  }
  noIoPrefix()

  val fire = io.cmd.fire
  val enable = Bool() setAsReg() init False
  val (_, _, cntOvf, addrNext) = utils.StridedAddrGen(
    inc = io.inc.toList,
    bound = io.bound.toList,
    addrWidth = addrWidth,
    enable = fire
  )

  val finish = cntOvf.reduceLeft(_ && _) && fire
  enable.setWhen(io.startPulse).clearWhen(finish)
  io.cmd.valid := enable
  io.busy := enable

  val addrAddUp = RegNextWhen(addrNext.reduceLeft(_ + _), fire, init = U(0))
  io.cmd.value := addrAddUp
  io.cmd.linked := io.fixLen
}
