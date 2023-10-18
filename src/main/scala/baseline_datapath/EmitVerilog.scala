package baseline_datapath

import chisel3._
import circt.stage.ChiselStage

class Foo extends Module {
  val in = IO(Input(Bool()))
  val in2 = IO(Input(Bool()))
  val out = IO(Output(Bool()))

  val inter = Wire(Bool())
  inter := in
  out := inter & in2
}

object EmitVerilog extends App {
  val str = ChiselStage.emitSystemVerilog(new Foo())
  print(str)
}
