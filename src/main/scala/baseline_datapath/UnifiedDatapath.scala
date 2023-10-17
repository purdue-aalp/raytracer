package baseline_datapath

import chisel3._
import hardfloat._
import chisel3.experimental.VecLiterals._ // for VecLit
import chisel3.util._
import chisel3.experimental.dataview._

class UnifiedDatapath extends Module {
  val in = IO(
    Flipped(Decoupled(new CombinedRayBoxTriangleBundle(recorded_float = false)))
  )
  val out = IO(
    Decoupled(new UnifiedDatapathOutput(recorded_float = false))
  )

  // always ready to accept jobs
  in.ready := WireDefault(true.B)
  out.valid := 0.U
  out.bits := 0.U.asTypeOf((out.bits))

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

  // Define the behavior of each stage as functions. 
  // Register their output to the "converyor belt" shift register.
  // Synopsys: stage_register(idx) := stage_function(idx)(stage_register(idx-1))

  // Each element of this array is an Option, that wraps a function object which
  // maps a Valid[ExtendedPipelineBundle] to a Valid[ExtendedPipelineBundle]
  val stage_functions = collection.mutable.ArrayBuffer.fill[Option[
    Valid[ExtendedPipelineBundle] => Valid[ExtendedPipelineBundle]
  ]](_stage_count)(None)

  // Define the functions!
  // stage 0 does not exist
  // stage 1 is register input
  // stage 2 is convert float32 to float33
  // stage 3 performs 24 adds for ray-box, or 9 adds for ray-triangle
  stage_functions(3) = Some({intake: Valid[ExtendedPipelineBundle] => 
    val emit = Valid(new ExtendedPipelineBundle(true))
    when(!intake.valid){
      emit := 0.U.asTypeOf(emit)
    }.elsewhen(intake.bits.isTriangleOp){
      // ray-triangle
    }.otherwise{
      // ray-box
      for (box_idx <- 0 until _box_plurality) {
        val _dest = Seq(
          emit.bits.aabb(box_idx).x_min,
          emit.bits.aabb(box_idx).y_min,
          emit.bits.aabb(box_idx).z_min,
          emit.bits.aabb(box_idx).x_max,
          emit.bits.aabb(box_idx).y_max,
          emit.bits.aabb(box_idx).z_max
        )
        val _src1 = Seq(
          intake.bits.aabb(box_idx).x_min,
          intake.bits.aabb(box_idx).y_min,
          intake.bits.aabb(box_idx).z_min,
          intake.bits.aabb(box_idx).x_max,
          intake.bits.aabb(box_idx).y_max,
          intake.bits.aabb(box_idx).z_max
        )
        val _src2 = Seq(
          intake.bits.ray.origin.x,
          intake.bits.ray.origin.y,
          intake.bits.ray.origin.z,
          intake.bits.ray.origin.x,
          intake.bits.ray.origin.y,
          intake.bits.ray.origin.z
        )
        (_dest zip _src1 zip _src2) foreach {
          case ((_1, _2), _3) =>
            val fu = Module(new AddRecFN(8, 24))
            fu.io.subOp := true.B
            fu.io.a := _2
            fu.io.b := _3
            fu.io.roundingMode := _rounding_rule
            fu.io.detectTininess := _tininess_rule
            _1 := fu.io.out
          // TODO: handle exception flags!
        }
      }
    }
    emit.valid := intake.valid
    emit
  })

  // The all-containing bundle that runs through all stages of the unified
  // pipeline. This is the conveyor belt between stages. 
  val stage_registers = Reg(
    Vec(_stage_count, Valid(new ExtendedPipelineBundle(recorded_float = true)))
  )
  // zero-th element is useless
  stage_registers(0) := 0.U.asTypeOf(stage_registers(0))

  // first element is useless too, because the first stage is "register input"
  // stage, the values are still non-recorded float. See
  // `stage_registers_1_actual` instead.
  stage_registers(1) := 0.U.asTypeOf(stage_registers(0))

  // chain up the stages!
  // Either apply the specified transformation, or default identity function, on
  // each stage's input
  for (idx <- 1 until _stage_count) {
    stage_registers(idx) := stage_functions(idx).getOrElse(
      (x: Valid[ExtendedPipelineBundle]) => identity(x)
    )(stage_registers(idx - 1))
  }

  // override the driver logic for stage_registers 1 and 2
  val stage_registers_1_actual = Reg(
    Valid(new CombinedRayBoxTriangleBundle(false))
  )
  stage_registers_1_actual.valid := in.fire
  stage_registers_1_actual.bits := in.bits

  stage_registers(2).valid := stage_registers_1_actual.valid
  stage_registers(
    2
  ).bits.isTriangleOp := stage_registers_1_actual.bits.isTriangleOp
  stage_registers(2).bits.ray := RayConvertFNtoRecFN(
    stage_registers_1_actual.bits.ray
  )
  (stage_registers(2).bits.aabb zip stage_registers_1_actual.bits.aabb).map {
    case (reg_2, reg_1) =>
      reg_2 := AABBConvertFNtoRecFN(reg_1)
  }
  stage_registers(2).bits.triangle := TriangleConvertFNtoRecFN(
    stage_registers_1_actual.bits.triangle
  )

}
