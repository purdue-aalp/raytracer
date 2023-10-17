package baseline_datapath

import chisel3._
import circt.stage.ChiselStage

object EmitVerilog extends App {
  val str = ChiselStage.emitSystemVerilog(new UnifiedDatapath())
  print(str)
}
