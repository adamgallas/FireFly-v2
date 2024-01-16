package systolic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Transfer2D(iterWidth: Int, addrWidth: Int) extends Component {
  val io = new Bundle {
    val eventIn = slave(Event)
    val addrOut = master(Stream(Fragment(UInt(addrWidth bits))))
  }
  val config = new Bundle {
    val bound1 = in UInt (iterWidth bits)
    val bound2 = in UInt (iterWidth bits)
    val stride1 = in UInt (addrWidth bits)
    val stride2 = in UInt (addrWidth bits)
    val baseAddr = in UInt (addrWidth bits)
  }
  noIoPrefix()

  val fire = io.eventIn.fire
  val cnt1 = UInt(iterWidth bits) setAsReg() init 0
  val cnt2 = UInt(iterWidth bits) setAsReg() init 0
  val cnt1Ovf = cnt1 === config.bound1
  val cnt2Ovf = cnt2 === config.bound2
  val addr1 = UInt(addrWidth bits) setAsReg() init 0
  val addr2 = UInt(addrWidth bits) setAsReg() init 0

  when(fire) {
    addr1 := addr1 + config.stride1
    when(cnt1Ovf) {
      addr1.clearAll()
      addr2 := addr2 + config.stride2
      when(cnt2Ovf) {
        addr2.clearAll()
      }
    }
  }

  when(fire) {
    cnt1 := cnt1 + 1
    when(cnt1Ovf) {
      cnt1.clearAll()
      cnt2 := cnt2 + 1
      when(cnt2Ovf) {
        cnt2.clearAll()
      }
    }
  }

  val localAddr = Stream(Fragment(UInt(addrWidth bits)))
  localAddr.arbitrationFrom(io.eventIn)
  localAddr.fragment := addr1 + addr2 + config.baseAddr
  localAddr.last := cnt1Ovf && cnt2Ovf
  io.addrOut << localAddr.m2sPipe()
}
