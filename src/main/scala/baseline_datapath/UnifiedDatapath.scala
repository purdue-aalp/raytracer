package baseline_datapath

import chisel3._
import hardfloat._
import chisel3.experimental.VecLiterals._ // for VecLit
import chisel3.util._

class UnifiedDatapath extends Module{
  val in = IO(
    Flipped(Decoupled(new CombinedRayBoxTriangleBundle(recorded_float = false)))
  )
  val out = IO(
    Decoupled(new UnifiedDatapathOutput(recorded_float = false))
  )

  // SOME CONSTANTS
  val _stage_count = 15;
  val _box_plurality =
    in.bits.aabb.length // this should be a Chisel elaboration-time known variable
  val _rounding_rule = consts.round_near_even
  val _tininess_rule = consts.tininess_beforeRounding
  val _zero_RecFN = {
    val convert_zero_int_to_zero_rec_float = Module(new INToRecFN(32, 8, 24))
    convert_zero_int_to_zero_rec_float.io.signedIn := false.B
    convert_zero_int_to_zero_rec_float.io.in := 0.U(32.W)
    convert_zero_int_to_zero_rec_float.io.roundingMode := _rounding_rule
    convert_zero_int_to_zero_rec_float.io.detectTininess := _tininess_rule
    val out_val = convert_zero_int_to_zero_rec_float.io.out
    out_val
  }
  val _neg_1_0_RecFN = {
    val convert_neg_one_int_to_neg_one_rec_float = Module(
      new INToRecFN(32, 8, 24)
    )
    convert_neg_one_int_to_neg_one_rec_float.io.signedIn := true.B
    convert_neg_one_int_to_neg_one_rec_float.io.in := -1.S(32.W).asUInt
    convert_neg_one_int_to_neg_one_rec_float.io.roundingMode := _rounding_rule
    convert_neg_one_int_to_neg_one_rec_float.io.detectTininess := _tininess_rule
    val out_val = convert_neg_one_int_to_neg_one_rec_float.io.out
    out_val
  }
  val _pos_1_0_RecFN = {
    val convert_pos_one_int_to_pos_one_rec_float = Module(
      new INToRecFN(32, 8, 24)
    )
    convert_pos_one_int_to_pos_one_rec_float.io.signedIn := true.B
    convert_pos_one_int_to_pos_one_rec_float.io.in := 1.S(32.W).asUInt
    convert_pos_one_int_to_pos_one_rec_float.io.roundingMode := _rounding_rule
    convert_pos_one_int_to_pos_one_rec_float.io.detectTininess := _tininess_rule
    val out_val = convert_pos_one_int_to_pos_one_rec_float.io.out
    out_val
  }
  // 0x7f800000 is positive inf in 32-bit float
  val _positive_inf_RecFN = recFNFromFN(8, 24, 0x7f800000.U)

  // The all-containing bundle that runs through all stages of the unified
  // pipeline. Initialize them like shift registers. Later code in this file
  // overwrites this definition. 
  val stage_registers = Reg(Vec(_stage_count,
    new ExtendedPipelineBundle(recorded_float = true)
  ))
  stage_registers(0) := 0.U.asTypeOf(stage_registers(0))

  // Define the behavior of each stage as functions. Register their output to
  // the "converyor belt" shift register
  val stage_functions = new collection.mutable.ArrayBuffer[ExtendedPipelineBundle=>ExtendedPipelineBundle](_stage_count)
  stage_functions(1) = {x=>???}
  stage_functions(2) = {???}

}