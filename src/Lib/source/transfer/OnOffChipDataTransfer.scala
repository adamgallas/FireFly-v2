package transfer

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class OnOffChipDataTransfer[TI <: Data, TO <: Data](
                                                         addrWidth: Int,
                                                         lenWidth: Int,
                                                         strideAddrGenWidth: Int,
                                                         inputDataType: HardType[TI],
                                                         outputDataType: HardType[TO],
                                                         widthOfBound: List[Int],
                                                         widthOfInc: List[Int]
                                                       ) extends Component {
  val io = new Bundle {
    val source = slave(Stream(inputDataType()))
    val dest = master(Stream(outputDataType()))
    val cmd = master(Stream(utils.Linked(UInt(addrWidth bits), UInt(lenWidth bits))))
    val start = in Bool()
    val busy = out Bool()
  }
  val config = new Bundle {
    val baseAddr = in UInt (addrWidth bits)
    val bound = in Vec widthOfBound.map(w => UInt(w bits))
    val inc = in Vec widthOfInc.map(w => UInt(w bits))
    val length = in UInt (lenWidth bits)
  }

  val cmd = Stream(utils.Linked(UInt(addrWidth bits), UInt(23 bits)))

  val cmdFire = cmd.fire
  val (cnt, addr, cntOvf, addrNext) = utils.StridedAddrGen(config.bound.toList, config.inc.toList, strideAddrGenWidth, cmdFire)
  val addrSum = RegNextWhen(addrNext.reduce(_ + _), cmdFire, init = U(0)) + config.baseAddr
  cmd.value := addrSum
  cmd.linked := config.length

  val enable = Bool() setAsReg() init false
  enable.setWhen(io.start).clearWhen(cntOvf.reduce(_ && _))
  io.busy := enable
  cmd.valid := enable

  StreamWidthAdapter(io.source, io.dest)
  io.cmd << cmd.m2sPipe()
}
