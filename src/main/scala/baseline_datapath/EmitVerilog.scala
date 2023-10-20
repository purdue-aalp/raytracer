package baseline_datapath

import chisel3._
import circt.stage.ChiselStage
import hardfloat._

class Foo extends Module {
  val in = IO(Input(Bits(33.W)))
  val in2 = IO(Input(Bits(33.W)))
  // val out = IO(Output(new RawFloat(8, 24)))
  val out2 = IO(Output(Bits(33.W)))

  // val inter = WireDefault(3.U(2.W))
  // out := resizeRawFloat(8, 22, in)
  // out := rawFloatFromRecFN(8, 24, in2)

  val fu = Module(new AddRecFN(8, 24))
  fu.io.a := in 
  fu.io.b := in2
  fu.io.subOp := false.B
  fu.io.roundingMode := consts.round_near_even
  fu.io.detectTininess := consts.tininess_beforeRounding
  out2 := fu.io.out
}

object EmitVerilog extends App {
  val str = ChiselStage.emitSystemVerilog(new Foo())
  print(str)
}
