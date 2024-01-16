package utils

import spinal.core._
import spinal.lib._

object EventM2sPipes {
  def apply(eventIn: Event, stages: Int): (Event, Vec[Bool]) = {
    val events = Vec(Event, stages)
    val CEs = Vec(events.map(_.ready))
    events.head.arbitrationFrom(eventIn)
    for (i <- 0 until stages - 1) {
      events(i + 1).arbitrationFrom(events(i).m2sPipe())
    }
    (events.last.m2sPipe(), CEs)
  }
}

case class EventM2sPipes(stages: Int) extends Component {
  val io = new Bundle {
    val eventIn = slave(Event)
    val eventOut = master(Event)
    val CEs = out Vec(Bool(), stages)
  }
  noIoPrefix()
  require(stages >= 1)
  val events = Vec(Event, stages)
  io.CEs := Vec(events.map(_.ready))
  events.head.arbitrationFrom(io.eventIn)
  for (i <- 0 until stages - 1) {
    events(i + 1).arbitrationFrom(events(i).m2sPipe())
  }
  io.eventOut.arbitrationFrom(events.last.m2sPipe())
}
