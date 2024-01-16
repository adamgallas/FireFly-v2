package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TransposeAddrGen(addrWidth: Int, pad: Int) extends Component {
  val io = new Bundle {
    val eventIn = slave(Event)
    val addrOut = master(Stream(Fragment(UInt(32 bits))))
  }
  val config = new Bundle {
    val height = in UInt (16 bits)
    val width = in UInt (16 bits)
    val stride = in UInt (addrWidth bits)
    val baseAddr = in UInt (32 bits)
  }
  val fire = io.eventIn.fire
  val cnth = UInt(16 bits) setAsReg() init 0
  val cntw = UInt(16 bits) setAsReg() init 0
  val addr = UInt(addrWidth bits) setAsReg() init 0
  val cnthOvf = cnth === config.height
  val cntwOvf = cntw === config.width

  when(fire) {
    cntw := cntw + 1
    addr := addr + config.stride
    when(cntwOvf) {
      cntw.clearAll()
      cnth := cnth + 1
      addr.clearAll()
      when(cnthOvf) {
        cnth.clearAll()
      }
    }
  }

  val localAddr = Stream(Fragment(UInt(addrWidth bits)))
  val localAddrPipe = localAddr.m2sPipe()
  localAddr.arbitrationFrom(io.eventIn)
  localAddr.fragment := addr + cnth
  localAddr.last := cnthOvf && cntwOvf

  val globalAddr = Stream(Fragment(UInt(32 bits)))
  globalAddr.arbitrationFrom(localAddrPipe)
  globalAddr.fragment := (localAddrPipe.fragment ## B(0, pad bits)).asUInt + config.baseAddr
  globalAddr.last := localAddrPipe.last

  io.addrOut << globalAddr.m2sPipe()
}
