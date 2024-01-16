package utils

import spinal.core._
import spinal.lib._

object AxiStreamSpecRenamer {
  def apply[T <: Bundle](that: T, widthT: Boolean = true): T = {
    def doIt() = {
      that.flatten.foreach(bt => {

        val t = if (widthT) "t" else ""
        bt.setName(bt.getName().replace("_payload_fragment", "_" + t + "data"))
        bt.setName(bt.getName().replace("_payload_last", "_" + t + "last"))

        bt.setName(bt.getName().replace("_fragment", "_" + t + "data"))
        bt.setName(bt.getName().replace("_payload", "_" + t + "data"))
        bt.setName(bt.getName().replace("_valid", "_" + t + "valid"))
        bt.setName(bt.getName().replace("_ready", "_" + t + "ready"))
        bt.setName(bt.getName().replace("_last", "_" + t + "last"))

        if (bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_", ""))
      })
    }

    if (Component.current == that.component)
      that.component.addPrePopTask(() => {
        doIt()
      })
    else
      doIt()
    that
  }
}