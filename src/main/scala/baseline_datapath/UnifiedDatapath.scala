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
  // out.valid := 0.U
  // out.bits := 0.U.asTypeOf((out.bits))

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

  // A cycle counter, that we can use when debugging the module
  // It shall disappear in synthesis, so no PPA impact
  val (_time, _) = Counter(true.B, (1 << 31) - 1)
  dontTouch(_time)

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
  stage_functions(3) = Some({ intake =>
    val emit = Wire(Valid(new ExtendedPipelineBundle(true)))

    // By default, copy the input. Code below overwrites fields of the bundle.
    emit := intake

    when(!intake.valid) {
      emit.bits := 0.U.asTypeOf(emit.bits)
    }.elsewhen(intake.bits.isTriangleOp) {
      // ray-triangle
    }.otherwise {
      // ray-box
      // the following implements the C-code:
      /*
        child.x_min = child.x_min - ray.origin.x;
        child.y_min = child.y_min - ray.origin.y;
        child.z_min = child.z_min - ray.origin.z;
        child.x_max = child.x_max - ray.origin.x;
        child.y_max = child.y_max - ray.origin.y;
        child.z_max = child.z_max - ray.origin.z;
       */
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
        (_dest zip _src1 zip _src2) foreach { case ((_1, _2), _3) =>
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

    emit
  })

  stage_functions(4) = Some({ intake =>
    val emit = Wire(Valid(new ExtendedPipelineBundle(true)))
    emit := intake

    when(!emit.valid) {
      emit.bits := 0.U.asTypeOf(emit.bits)
    }.elsewhen(intake.bits.isTriangleOp) {}.otherwise {
      // the following implements the C-code
      /*
        float tp_min_x = child.x_min * ray.inv.x;
        float tp_min_y = child.y_min * ray.inv.y;
        float tp_min_z = child.z_min * ray.inv.z;
        float tp_max_x = child.x_max * ray.inv.x;
        float tp_max_y = child.y_max * ray.inv.y;
        float tp_max_z = child.z_max * ray.inv.z;
       */
      for (box_idx <- 0 until _box_plurality) {
        val _dest = Seq(
          emit.bits.t_min(box_idx).x,
          emit.bits.t_min(box_idx).y,
          emit.bits.t_min(box_idx).z,
          emit.bits.t_min(box_idx).x,
          emit.bits.t_min(box_idx).y,
          emit.bits.t_min(box_idx).z
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
          intake.bits.ray.inv.x,
          intake.bits.ray.inv.y,
          intake.bits.ray.inv.z,
          intake.bits.ray.inv.x,
          intake.bits.ray.inv.y,
          intake.bits.ray.inv.z
        )
        (_dest zip _src1 zip _src2) foreach { case ((_1, _2), _3) =>
          val fu = Module(new MulRecFN(8, 24))
          fu.io.a := _2
          fu.io.b := _3
          fu.io.roundingMode := _rounding_rule
          fu.io.detectTininess := _tininess_rule
          _1 := fu.io.out
        // TODO: handle exception flags!
        }
      }
    }

    emit
  })

  stage_functions(10) = Some({ intake =>
    val emit = Wire(Valid(new ExtendedPipelineBundle(true)))
    emit := intake

    when(!emit.valid) {
      emit.bits := 0.U.asTypeOf(emit.bits)
    }.elsewhen(emit.bits.isTriangleOp) {}.otherwise {
      def flip_intervals_if_dir_is_neg(
          c_out: Bits,
          d_out: Bits,
          a: Bits,
          b: Bits,
          c: Bits,
          d: Bits,
          dont_flip_if_equal: Boolean // scala type boolean, not Chisel type!
      ) = {
        val fu = Module(
          new RecFNCompareSelect(
            option = dont_flip_if_equal,
            passthrough_type = Bits(33.W)
          )
        )
        c_out := fu.io.c_out
        d_out := fu.io.d_out
        fu.io.a := a
        fu.io.b := b
        fu.io.c := c
        fu.io.d := d
      }

      val isIntersect_intermediate = Wire(Vec(_box_plurality, Bool()))
      val tmin_intermediate = Wire(Vec(_box_plurality, Bits(33.W)))
      val tmax_intermediate = Wire(Vec(_box_plurality, Bits(33.W)))

      for (box_idx <- 0 until _box_plurality) {
        flip_intervals_if_dir_is_neg(
          emit.bits.t_min(box_idx).x,
          emit.bits.t_max(box_idx).x,
          intake.bits.ray.dir.x,
          _zero_RecFN,
          intake.bits.t_min(box_idx).x,
          intake.bits.t_max(box_idx).x,
          true
        )
        flip_intervals_if_dir_is_neg(
          emit.bits.t_min(box_idx).y,
          emit.bits.t_max(box_idx).y,
          intake.bits.ray.dir.y,
          _zero_RecFN,
          intake.bits.t_min(box_idx).y,
          intake.bits.t_max(box_idx).y,
          true
        )
        flip_intervals_if_dir_is_neg(
          emit.bits.t_min(box_idx).z,
          emit.bits.t_max(box_idx).z,
          intake.bits.ray.dir.z,
          _zero_RecFN,
          intake.bits.t_min(box_idx).z,
          intake.bits.t_max(box_idx).z,
          true
        )

        val quad_sort_for_tmin = Module(new QuadSortRecFN())
        quad_sort_for_tmin.io.in(0) := emit.bits.t_min(box_idx).x
        quad_sort_for_tmin.io.in(1) := emit.bits.t_min(box_idx).y
        quad_sort_for_tmin.io.in(2) := emit.bits.t_min(box_idx).z
        quad_sort_for_tmin.io.in(3) := _zero_RecFN
        tmin_intermediate(box_idx) := quad_sort_for_tmin.io.largest

        val quad_sort_for_tmax = Module(new QuadSortRecFN())
        quad_sort_for_tmax.io.in(0) := emit.bits.t_max(box_idx).x
        quad_sort_for_tmax.io.in(1) := emit.bits.t_max(box_idx).y
        quad_sort_for_tmax.io.in(2) := emit.bits.t_max(box_idx).z
        quad_sort_for_tmax.io.in(3) := intake.bits.ray.extent
        tmax_intermediate(box_idx) := quad_sort_for_tmax.io.smallest

        // if there's overlap between [tmin, inf) and (-inf, tmax], we say ray-box
        // intersection happens
        val comp_tmin_tmax = Module(new CompareRecFN(8, 24))
        comp_tmin_tmax.io.a := tmin_intermediate(box_idx)
        comp_tmin_tmax.io.b := tmax_intermediate(box_idx)
        comp_tmin_tmax.io.signaling := true.B

        isIntersect_intermediate(box_idx) := comp_tmin_tmax.io.lt
      }

      val tmin_but_substitute_non_intersect_with_PInf =
        tmin_intermediate.zip(isIntersect_intermediate).map {
          case (rec_fn, b) =>
            Mux(b, rec_fn, _positive_inf_RecFN)
        }

      // sorter outputs from biggest to smallest, so reverse output
      val quad_sort_for_box_index = Module(new QuadSortRecFNWithIndex)
      quad_sort_for_box_index.io.in := tmin_but_substitute_non_intersect_with_PInf
      emit.bits.boxIndex := quad_sort_for_box_index.io.sorted_indices.reverse

      val quad_sort_for_t = Module(new QuadSortRecFN)
      quad_sort_for_t.io.in := tmin_but_substitute_non_intersect_with_PInf
      emit.bits.tmin := quad_sort_for_t.io.out.reverse

      emit.bits.isIntersect.zip(emit.bits.boxIndex).foreach { case (b, idx) =>
        b := isIntersect_intermediate(idx)
      }

    }

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

  // TAP OUTPUT FROM LAST MEANINGFUL STAGE'S REGISTER
  out.bits := 0.U.asTypeOf((out.bits))
  out.valid := stage_registers(10).valid
  out.bits.isTriangleOp := stage_registers(10).bits.isTriangleOp
  out.bits.tmin_out := stage_registers(10).bits.tmin.map(fNFromRecFN(8, 24, _))
  out.bits.isIntersect := stage_registers(10).bits.isIntersect
  out.bits.boxIndex := stage_registers(10).bits.boxIndex
}
