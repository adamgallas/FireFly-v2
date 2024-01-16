package systolic.sparsev2

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class Ram4RW[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  val addressWidth = log2Up(depth)

  val io = new Bundle {
    val write = slave(Flow(Fragment(Vec(dataType, 4))))
    val readCmd = Vec(slave(Flow(UInt(addressWidth bits))), 4)
    val readData = Vec(master(Flow(dataType)), 4)
  }
  val ram = Array.fill(2)(Mem(dataType, depth / 2))
  val xorRam = Mem(Bits(dataType.getBitsWidth bits), depth / 2)

  val writeCnt = UInt(addressWidth - 1 bits) setAsReg() init 0
  val writeCnt0 = writeCnt
  val writeCnt1 = writeCnt + 1
  val writeXor0 = io.write.fragment(0).asBits ^ io.write.fragment(1).asBits
  val writeXor1 = io.write.fragment(0).asBits ^ io.write.fragment(1).asBits
  when(io.write.valid) {
    writeCnt := writeCnt + 2
    when(io.write.last) {
      writeCnt.clearAll()
    }
  }

  val readOnly = new Area {
    val sel = Vec(io.readCmd.take(2).map(_.payload.take(1))).asBits.asUInt
    val selDly = RegNext(sel)
    val ram0AddrR = sel.mux(mappings =
      1 -> io.readCmd(1).payload.drop(1).asUInt,
      default -> io.readCmd(0).payload.drop(1).asUInt
    )
    val ram1AddrR = sel.mux(mappings =
      1 -> io.readCmd(0).payload.drop(1).asUInt,
      default -> io.readCmd(1).payload.drop(1).asUInt
    )
    val xorAddrR = sel.mux(mappings =
      0 -> io.readCmd(1).payload.drop(1).asUInt,
      default -> io.readCmd(0).payload.drop(1).asUInt
    )

    val wen = io.write.valid
    val xorAddr = Mux(wen, writeCnt0, xorAddrR)
    val ram0Addr = Mux(wen, writeCnt0, ram0AddrR)
    val ram1Addr = Mux(wen, writeCnt0, ram1AddrR)

    val xorOut = xorRam.readWriteSync(xorAddr, writeXor0, True, wen)
    val ram0Out = ram(0).readWriteSync(ram0Addr, io.write.fragment(0), True, wen)
    val ram1Out = ram(1).readWriteSync(ram1Addr, io.write.fragment(1), True, wen)

    val out0 = selDly.mux(mappings =
      0 -> ram0Out,
      1 -> ram1Out,
      2 -> ram0Out,
      3 -> (ram0Out.asBits ^ xorOut.asBits).as(dataType)
    )
    val out1 = selDly.mux(mappings =
      0 -> (ram1Out.asBits ^ xorOut.asBits).as(dataType),
      1 -> ram0Out,
      2 -> ram1Out,
      3 -> ram1Out
    )

    io.readData(0).payload := RegNext(out0)
    io.readData(1).payload := RegNext(out1)
    io.readData(0).valid := Delay(io.readCmd(0).valid, 2, init = False)
    io.readData(1).valid := Delay(io.readCmd(1).valid, 2, init = False)
  }

  val readWrite1 = new Area {
    val sel = Vec(io.readCmd.takeRight(2).map(_.payload.take(1))).asBits.asUInt
    val selDly = RegNext(sel)
    val ram0AddrR = sel.mux(mappings =
      1 -> io.readCmd(3).payload.drop(1).asUInt,
      default -> io.readCmd(2).payload.drop(1).asUInt
    )
    val ram1AddrR = sel.mux(mappings =
      1 -> io.readCmd(2).payload.drop(1).asUInt,
      default -> io.readCmd(3).payload.drop(1).asUInt
    )
    val xorAddrR = sel.mux(mappings =
      0 -> io.readCmd(3).payload.drop(1).asUInt,
      default -> io.readCmd(2).payload.drop(1).asUInt
    )

    val wen = io.write.valid
    val xorAddr = Mux(wen, writeCnt1, xorAddrR)
    val ram0Addr = Mux(wen, writeCnt1, ram0AddrR)
    val ram1Addr = Mux(wen, writeCnt1, ram1AddrR)

    val xorOut = xorRam.readWriteSync(xorAddr, writeXor1, True, wen)
    val ram0Out = ram(0).readWriteSync(ram0Addr, io.write.fragment(2), True, wen)
    val ram1Out = ram(1).readWriteSync(ram1Addr, io.write.fragment(3), True, wen)

    val out0 = selDly.mux(mappings =
      0 -> ram0Out,
      1 -> ram1Out,
      2 -> ram0Out,
      3 -> (ram0Out.asBits ^ xorOut.asBits).as(dataType)
    )
    val out1 = selDly.mux(mappings =
      0 -> (ram1Out.asBits ^ xorOut.asBits).as(dataType),
      1 -> ram0Out,
      2 -> ram1Out,
      3 -> ram1Out
    )

    io.readData(2).payload := RegNext(out0)
    io.readData(3).payload := RegNext(out1)
    io.readData(2).valid := Delay(io.readCmd(2).valid, 2, init = False)
    io.readData(3).valid := Delay(io.readCmd(3).valid, 2, init = False)
  }
}

object Ram4RW extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    targetDirectory = "data//verilog//Lib",
    anonymSignalPrefix = "tmp"
  ).generateVerilog(
    new Ram4RW(Bits(64 bits), 2048)
  )
}
