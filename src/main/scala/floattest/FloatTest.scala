package floattest

import chisel3._
import hardfloat._

class FloatCalc extends Module {
  val io = IO(new Bundle {
    val in1 = Input(SInt(32.W))
    val in2 = Input(SInt(32.W))
    val out = Output(UInt(32.W))
  })

  // convert input from Int to Recorded Float
  val in1_recfn = Module(new INToRecFN(32, 8, 24))
  in1_recfn.io.signedIn := true.B
  in1_recfn.io.in := io.in1.asUInt
  in1_recfn.io.roundingMode := consts.round_min 
  in1_recfn.io.detectTininess := consts.tininess_afterRounding 

  val in2_recfn = Module(new INToRecFN(32, 8, 24))
  in2_recfn.io.signedIn := true.B
  in2_recfn.io.in := io.in2.asUInt
  in2_recfn.io.roundingMode := consts.round_min 
  in2_recfn.io.detectTininess := consts.tininess_afterRounding 

  // functional unit does the calculation
  val fu = Module(new AddRecFN(8, 24))
  fu.io.subOp := false.B 
  fu.io.a := in1_recfn.io.out 
  fu.io.b := in2_recfn.io.out 
  fu.io.roundingMode := consts.round_min
  fu.io.detectTininess := consts.tininess_afterRounding 

  // convert output to IEEE 754-compliant Float from Recorded Float 
  io.out := fNFromRecFN(8, 24, fu.io.out)
}