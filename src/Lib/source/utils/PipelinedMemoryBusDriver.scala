package utils

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.simple.PipelinedMemoryBus

case class PipelinedMemoryBusDriver(bus: PipelinedMemoryBus, clockDomain: ClockDomain) {
  def reset(): Unit = {
    bus.cmd.valid #= false
    bus.cmd.write #= false
    bus.cmd.address #= 0
    bus.cmd.data #= 0
  }

  def read(address: BigInt): BigInt = {
    bus.cmd.valid #= true
    bus.cmd.payload.address #= address
    bus.cmd.ready #= true
    clockDomain.waitSamplingWhere(bus.cmd.ready.toBoolean)
    bus.cmd.valid #= false
    bus.cmd.payload.data.toBigInt
  }

  def write(address: BigInt, data: BigInt): Unit = {
    bus.cmd.valid #= true
    bus.cmd.payload.address #= address
    bus.cmd.write #= true
    bus.cmd.payload.data #= data
    clockDomain.waitSamplingWhere(bus.cmd.ready.toBoolean)
    bus.cmd.valid #= false
    bus.cmd.write #= false
  }
}