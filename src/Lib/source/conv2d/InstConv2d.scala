package conv2d

import spinal.core._

import scala.language.postfixOps

object InstConv2d extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    targetDirectory = "data//verilog//Lib",
    anonymSignalPrefix = ""
  ).generateVerilog(
    new Conv2d(Bits(16 * 8 bits), 8, 1024)
  )
}
