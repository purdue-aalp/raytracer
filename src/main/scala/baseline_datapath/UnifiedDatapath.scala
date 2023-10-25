package baseline_datapath

import chisel3._
import hardfloat._
import chisel3.experimental.VecLiterals._ // for VecLit
import chisel3.util._
import chisel3.experimental.dataview._

object DatapathConstants {
  def _zero_RecFN = recFNFromFN(8, 24, 0x0.U)
  def _neg_1_0_RecFN = recFNFromFN(8, 24, 0xbf800000.S)
  def _pos_1_0_RecFN = recFNFromFN(8, 24, 0x3f800000.S)
  // 0x7f800000 is positive inf in 32-bit float
  def _positive_inf_RecFN = recFNFromFN(8, 24, 0x7f800000.S)
}

class UnifiedDatapath(submodule_for_stage: Boolean = true) extends Module {
  import DatapathConstants._

  // input is guaranteed to be registered by this module
  val in = IO(
    Flipped(Decoupled(new CombinedRayBoxTriangleBundle(recorded_float = false)))
  )

  // output is not registered, it is driven by logic
  val out = IO(
    Decoupled(new UnifiedDatapathOutput(recorded_float = false))
  )

  // always ready to accept jobs each cycle
  in.ready := WireDefault(true.B)

  // A cycle counter, that we can use when debugging the module
  // It shall disappear in synthesis, so no PPA impact
  val (_time, _) = Counter(true.B, (1 << 31) - 1)
  dontTouch(_time)

  /////////////////
  // SOME CONSTANTS
  /////////////
  val _stage_count = 12;
  val _box_plurality =
    in.bits.aabb.length // this should be a Chisel elaboration-time known variable
  val _rounding_rule = consts.round_near_even
  val _tininess_rule = consts.tininess_beforeRounding

  /////////////////
  // STAGE BEHAVIOR
  ///////////////////

  // Define the behavior of each stage as functions.
  // Register their output to the "converyor belt" shift register.
  // Synopsys: stage_register(idx) := stage_function(idx)(stage_register(idx-1))

  // Each element of this array is an Option, that wraps a function object which
  // maps a Valid[ExtendedPipelineBundle] to a Valid[ExtendedPipelineBundle]
  val stage_functions = collection.mutable.ArrayBuffer.fill[Option[
    ExtendedPipelineBundle => ExtendedPipelineBundle
  ]](_stage_count)(None)

  // Define the functions!
  // stage 0 does not exist
  // stage 1 is register input
  // stage 2 is convert float32 to float33
  // stage 3 performs 24 adds for ray-box, or 9 adds for ray-triangle, to
  // translate the geometries to the origin of the ray
  stage_functions(3) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))

    // By default, copy the input. Code below overwrites fields of the bundle.
    emit := intake

    // an infinite list of AddRecFN modules, but only instantiated when needed
    val fu_list: List[AddRecFN] = List.fill(24) {
      val fu = Module(new AddRecFN(8, 24))
      fu.io.a := 0.U
      fu.io.b := 0.U
      fu.io.detectTininess := 0.U
      fu.io.roundingMode := 0.U
      fu.io.subOp := 0.U
      fu
    }

    when(intake.isTriangleOp) {
      // ray-triangle
      val _dest = Seq(
        emit.triangle.A.x,
        emit.triangle.A.y,
        emit.triangle.A.z,
        emit.triangle.B.x,
        emit.triangle.B.y,
        emit.triangle.B.z,
        emit.triangle.C.x,
        emit.triangle.C.y,
        emit.triangle.C.z
      )

      val _src1 = Seq(
        intake.triangle.A.x,
        intake.triangle.A.y,
        intake.triangle.A.z,
        intake.triangle.B.x,
        intake.triangle.B.y,
        intake.triangle.B.z,
        intake.triangle.C.x,
        intake.triangle.C.y,
        intake.triangle.C.z
      )

      val _src2 = Seq(
        intake.ray.origin.x,
        intake.ray.origin.y,
        intake.ray.origin.z,
        intake.ray.origin.x,
        intake.ray.origin.y,
        intake.ray.origin.z,
        intake.ray.origin.x,
        intake.ray.origin.y,
        intake.ray.origin.z
      )

      (_dest zip _src1 zip _src2 zip fu_list) foreach {
        case (((_1, _2), _3), fu) =>
          fu.io.subOp := true.B
          fu.io.a := _2
          fu.io.b := _3
          fu.io.roundingMode := _rounding_rule
          fu.io.detectTininess := _tininess_rule
          _1 := fu.io.out
        // TODO: handle exception flags!
      }
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
      val _dest = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          emit.aabb(box_idx).x_min,
          emit.aabb(box_idx).y_min,
          emit.aabb(box_idx).z_min,
          emit.aabb(box_idx).x_max,
          emit.aabb(box_idx).y_max,
          emit.aabb(box_idx).z_max
        )
      }
      val _src1 = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          intake.aabb(box_idx).x_min,
          intake.aabb(box_idx).y_min,
          intake.aabb(box_idx).z_min,
          intake.aabb(box_idx).x_max,
          intake.aabb(box_idx).y_max,
          intake.aabb(box_idx).z_max
        )
      }
      val _src2 = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          intake.ray.origin.x,
          intake.ray.origin.y,
          intake.ray.origin.z,
          intake.ray.origin.x,
          intake.ray.origin.y,
          intake.ray.origin.z
        )
      }

      (_dest zip _src1 zip _src2 zip fu_list) foreach {
        case (((_1, _2), _3), fu) =>
          fu.io.subOp := true.B
          fu.io.a := _2
          fu.io.b := _3
          fu.io.roundingMode := _rounding_rule
          fu.io.detectTininess := _tininess_rule
          _1 := fu.io.out
        // TODO: handle exception flags!
      }
    }

    emit
  })

  // stage 4 performs 24 mul-adds for ray-box to find out the time intersection
  // intervals, or 9 mul-adds for ray-triangle to perform shear and scale of
  // triangle vertices
  stage_functions(4) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    val fu_list: List[MulAddRecFN] = List.fill(24) {
      val fu = Module(new MulAddRecFN(8, 24))
      fu.io.a := 0.U
      fu.io.b := 0.U
      fu.io.c := 0.U
      fu.io.detectTininess := 0.U
      fu.io.roundingMode := 0.U
      fu.io.op := 0.U
      fu
    }

    when(intake.isTriangleOp) {
      val kx = intake.ray.kx
      val ky = intake.ray.ky
      val kz = intake.ray.kz

      val _dest = Seq(
        emit.A.x,
        emit.A.y,
        emit.A.z,
        emit.B.x,
        emit.B.y,
        emit.B.z,
        emit.C.x,
        emit.C.y,
        emit.C.z
      )

      val _src1 = Seq(
        FNFlipSign(intake.ray.shear.x),
        FNFlipSign(intake.ray.shear.y),
        intake.ray.shear.z,
        FNFlipSign(intake.ray.shear.x),
        FNFlipSign(intake.ray.shear.y),
        intake.ray.shear.z,
        FNFlipSign(intake.ray.shear.x),
        FNFlipSign(intake.ray.shear.y),
        intake.ray.shear.z
      )

      val _src2 = Seq(
        intake.triangle.A.at(kz),
        intake.triangle.A.at(kz),
        intake.triangle.A.at(kz),
        intake.triangle.B.at(kz),
        intake.triangle.B.at(kz),
        intake.triangle.B.at(kz),
        intake.triangle.C.at(kz),
        intake.triangle.C.at(kz),
        intake.triangle.C.at(kz)
      )

      val _src3 = Seq(
        intake.triangle.A.at(kx),
        intake.triangle.A.at(ky),
        _zero_RecFN,
        intake.triangle.B.at(kx),
        intake.triangle.B.at(ky),
        _zero_RecFN,
        intake.triangle.C.at(kx),
        intake.triangle.C.at(ky),
        _zero_RecFN
      )

      // _dest = _src1 * _src2 + _src3
      (_dest zip _src1 zip _src2 zip _src3 zip fu_list).map {
        case ((((_1, _2), _3), _4), fu) =>
          // val fu = Module(new MulAddRecFN(8, 24))
          fu.io.op := 0.U
          fu.io.a := _2
          fu.io.b := _3
          fu.io.c := _4
          fu.io.roundingMode := _rounding_rule
          fu.io.detectTininess := _tininess_rule

          _1 := fu.io.out
        // handle exception flags!
      }

    }.otherwise {
      // the following implements the C-code
      /*
        float tp_min_x = child.x_min * ray.inv.x;
        float tp_min_y = child.y_min * ray.inv.y;
        float tp_min_z = child.z_min * ray.inv.z;
        float tp_max_x = child.x_max * ray.inv.x;
        float tp_max_y = child.y_max * ray.inv.y;
        float tp_max_z = child.z_max * ray.inv.z;
       */
      val _dest = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          emit.t_min(box_idx).x,
          emit.t_min(box_idx).y,
          emit.t_min(box_idx).z,
          emit.t_max(box_idx).x,
          emit.t_max(box_idx).y,
          emit.t_max(box_idx).z
        )
      }
      val _src1 = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          intake.aabb(box_idx).x_min,
          intake.aabb(box_idx).y_min,
          intake.aabb(box_idx).z_min,
          intake.aabb(box_idx).x_max,
          intake.aabb(box_idx).y_max,
          intake.aabb(box_idx).z_max
        )
      }

      val _src2 = (0 until _box_plurality).flatMap { box_idx =>
        Seq(
          intake.ray.inv.x,
          intake.ray.inv.y,
          intake.ray.inv.z,
          intake.ray.inv.x,
          intake.ray.inv.y,
          intake.ray.inv.z
        )
      }

      (_dest zip _src1 zip _src2 zip fu_list) foreach {
        case (((_1, _2), _3), fu) =>
          // val fu = Module(new MulRecFN(8, 24))
          // fu.io.a := _2
          // fu.io.b := _3
          // fu.io.roundingMode := _rounding_rule
          // fu.io.detectTininess := _tininess_rule
          // _1 := fu.io.out
          // val fu = Module(new MulAddRecFN(8, 24))
          fu.io.op := 0.U
          fu.io.a := _2
          fu.io.b := _3
          fu.io.c := _zero_RecFN
          fu.io.roundingMode := _rounding_rule
          fu.io.detectTininess := _tininess_rule

          _1 := fu.io.out
        // TODO: handle exception flags!
      }

    }

    emit
  })

  // stage 5 performs 6 muls for ray-triangle test to figure out the minuends and
  // subtrahends of U, V, W
  stage_functions(5) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    when(intake.isTriangleOp) {
      val _dest = Seq(
        emit.U,
        emit.V,
        emit.W,
        emit.U_subtrahend,
        emit.V_subtrahend,
        emit.W_subtrahend
      )

      val _src1 = Seq(
        intake.C.x,
        intake.A.x,
        intake.B.x,
        intake.C.y,
        intake.A.y,
        intake.B.y
      )

      val _src2 = Seq(
        intake.B.y,
        intake.C.y,
        intake.A.y,
        intake.B.x,
        intake.C.x,
        intake.A.x
      )

      (_dest zip _src1 zip _src2).map { case ((_1, _2), _3) =>
        val fu = Module(new MulRecFN(8, 24))
        fu.io.a := _2
        fu.io.b := _3
        fu.io.roundingMode := _rounding_rule
        fu.io.detectTininess := _tininess_rule

        _1 := fu.io.out
      // handle exception flags!
      }
    }.otherwise {}

    emit
  })

  // stage 6 performs 3 adds for ray-triangle test to find the value of U, V, W
  stage_functions(6) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    when(intake.isTriangleOp) {
      val _dest = Seq(
        emit.U,
        emit.V,
        emit.W
      )
      val _src1 = Seq(
        intake.U,
        intake.V,
        intake.W
      )
      val _src2 = Seq(
        intake.U_subtrahend,
        intake.V_subtrahend,
        intake.W_subtrahend
      )
      (_dest zip _src1 zip _src2).map { case ((_1, _2), _3) =>
        val fu = Module(new AddRecFN(8, 24))
        fu.io.subOp := true.B
        fu.io.a := _2
        fu.io.b := _3
        fu.io.roundingMode := _rounding_rule
        fu.io.detectTininess := _tininess_rule

        _1 := fu.io.out
      // handle exception flags!
      }

    }.otherwise {}

    emit
  })

  // stage 7 calculates U*Az, V*Bz and W*Cz for ray-triangle test
  stage_functions(7) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    when(intake.isTriangleOp) {
      val _dest = Seq(
        emit.U_Az,
        emit.V_Bz,
        emit.W_Cz
      )
      val _src1 = Seq(
        intake.U,
        intake.V,
        intake.W
      )
      val _src2 = Seq(
        intake.A.z,
        intake.B.z,
        intake.C.z
      )
      (_dest zip _src1 zip _src2).map { case ((_1, _2), _3) =>
        val fu = Module(new MulRecFN(8, 24))
        fu.io.a := _2
        fu.io.b := _3
        fu.io.roundingMode := _rounding_rule
        fu.io.detectTininess := _tininess_rule

        _1 := fu.io.out
      // handle exception flags
      }
    }.otherwise {}

    emit
  })

  // stage 8 does two adds for ray-triangle test to find out the partial sum for
  // t_denom and t_num
  stage_functions(8) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    when(intake.isTriangleOp) {
      val fu_denom = Module(new AddRecFN(8, 24))
      fu_denom.io.subOp := false.B
      fu_denom.io.a := intake.U
      fu_denom.io.b := intake.V
      fu_denom.io.roundingMode := _rounding_rule
      fu_denom.io.detectTininess := _tininess_rule
      emit.t_denom := fu_denom.io.out
      // handle exception flags

      val fu_num = Module(new AddRecFN(8, 24))
      fu_num.io.subOp := false.B
      fu_num.io.a := intake.U_Az
      fu_num.io.b := intake.V_Bz
      fu_num.io.roundingMode := _rounding_rule
      fu_num.io.detectTininess := _tininess_rule
      emit.t_num := fu_num.io.out
      // handle exception flags
    }.otherwise {}

    emit
  })

  // stage 9 performs two adds for ray-triangle test, to complete the summation
  // of t_denom and t_num
  stage_functions(9) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    when(intake.isTriangleOp) {
      val fu_denom = Module(new AddRecFN(8, 24))
      fu_denom.io.subOp := false.B
      fu_denom.io.a := intake.t_denom
      fu_denom.io.b := intake.W
      fu_denom.io.roundingMode := _rounding_rule
      fu_denom.io.detectTininess := _tininess_rule
      emit.t_denom := fu_denom.io.out
      // handle exception flags

      val fu_num = Module(new AddRecFN(8, 24))
      fu_num.io.subOp := false.B
      fu_num.io.a := intake.t_num
      fu_num.io.b := intake.W_Cz
      fu_num.io.roundingMode := _rounding_rule
      fu_num.io.detectTininess := _tininess_rule
      emit.t_num := fu_num.io.out
      // handle exception flags
    }.otherwise {}

    emit
  })

  // stage 10 performs 12 CAS, 10 quad-sorts and 4 comparisons for ray-box
  // intersection to figure out intersecting boxes and sort them; or 5
  // comparisons for ray-triangle test to determine validity of intersection
  stage_functions(10) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    // printf(cf"${_time}: stage 10 intake is valid? ${intake.valid}\n")

    when(intake.isTriangleOp) {
      val _src1 = Seq(
        intake.U,
        intake.V,
        intake.W,
        intake.t_denom,
        intake.t_num
      )
      val _src2 = Seq.fill(5)(_zero_RecFN)
      val _fu = Seq.fill(5)(Module(new CompareRecFN(8, 24)))

      (_src1 zip _src2 zip _fu).map { case ((_1, _2), _3) =>
        _3.io.a := _1
        _3.io.b := _2
        _3.io.signaling := true.B
      }

      when(_fu(0).io.lt || _fu(1).io.lt || _fu(2).io.lt || _fu(4).io.lt) {
        // miss if U/V/W/t_num < 0.0f
        emit.triangle_hit := false.B
      }.elsewhen(_fu(3).io.eq) {
        // miss if t_denom == 0.0f
        emit.triangle_hit := false.B
      }.otherwise {
        emit.triangle_hit := true.B
      }
    }.otherwise {
      // ray-box test
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
          emit.t_min(box_idx).x,
          emit.t_max(box_idx).x,
          intake.ray.dir.x,
          _zero_RecFN,
          intake.t_min(box_idx).x,
          intake.t_max(box_idx).x,
          true
        )
        flip_intervals_if_dir_is_neg(
          emit.t_min(box_idx).y,
          emit.t_max(box_idx).y,
          intake.ray.dir.y,
          _zero_RecFN,
          intake.t_min(box_idx).y,
          intake.t_max(box_idx).y,
          true
        )
        flip_intervals_if_dir_is_neg(
          emit.t_min(box_idx).z,
          emit.t_max(box_idx).z,
          intake.ray.dir.z,
          _zero_RecFN,
          intake.t_min(box_idx).z,
          intake.t_max(box_idx).z,
          true
        )

        val quad_sort_for_tmin = Module(new QuadSortRecFN())
        quad_sort_for_tmin.io.in(0) := emit.t_min(box_idx).x
        quad_sort_for_tmin.io.in(1) := emit.t_min(box_idx).y
        quad_sort_for_tmin.io.in(2) := emit.t_min(box_idx).z
        quad_sort_for_tmin.io.in(3) := _zero_RecFN
        tmin_intermediate(box_idx) := quad_sort_for_tmin.io.largest

        val quad_sort_for_tmax = Module(new QuadSortRecFN())
        quad_sort_for_tmax.io.in(0) := emit.t_max(box_idx).x
        quad_sort_for_tmax.io.in(1) := emit.t_max(box_idx).y
        quad_sort_for_tmax.io.in(2) := emit.t_max(box_idx).z
        quad_sort_for_tmax.io.in(3) := intake.ray.extent
        tmax_intermediate(box_idx) := quad_sort_for_tmax.io.smallest

        // if there's overlap between [tmin, inf) and (-inf, tmax], we say ray-box
        // intersection happens
        val comp_tmin_tmax = Module(new CompareRecFN(8, 24))
        comp_tmin_tmax.io.a := tmin_intermediate(box_idx)
        comp_tmin_tmax.io.b := tmax_intermediate(box_idx)
        comp_tmin_tmax.io.signaling := true.B

        isIntersect_intermediate(box_idx) := comp_tmin_tmax.io.lt
      }
      // printf(cf"${_time}: tpmin is ${emit.t_min}, tpmax is ${emit.t_max}\n")
      // printf(cf"${_time}: tmin_intermediate is ${tmin_intermediate}\n")

      val tmin_but_substitute_non_intersect_with_PInf =
        tmin_intermediate.zip(isIntersect_intermediate).map {
          case (rec_fn, b) =>
            Mux(b, rec_fn, _positive_inf_RecFN)
        }

      // sorter outputs from biggest to smallest, so reverse output
      val quad_sort_for_box_index = Module(new QuadSortRecFNWithIndex)
      quad_sort_for_box_index.io.in := tmin_but_substitute_non_intersect_with_PInf
      emit.boxIndex := quad_sort_for_box_index.io.sorted_indices.reverse

      val quad_sort_for_t = Module(new QuadSortRecFN)
      quad_sort_for_t.io.in := tmin_but_substitute_non_intersect_with_PInf
      emit.tmin := quad_sort_for_t.io.out.reverse

      emit.isIntersect.zip(emit.boxIndex).foreach { case (b, idx) =>
        b := isIntersect_intermediate(idx)
      }
    }

    emit
  })

  //////////////////
  // STAGE REGISTERS
  ///////////////////

  // This is the conveyor belt between stages.
  // val stage_registers = Reg(
  //   Vec(_stage_count, Valid(new ExtendedPipelineBundle(recorded_float = true)))
  // )

  // chain up the stages!
  // Either apply the specified transformation, or default identity function, on
  // each stage's input
  // for (idx <- 1 until _stage_count) {
  //   if (!submodule_for_stage) {
  //     stage_registers(idx).bits := stage_functions(idx).getOrElse(
  //       (x: ExtendedPipelineBundle) => identity(x)
  //     )(stage_registers(idx - 1).bits)
  //     stage_registers(idx).valid := stage_registers(idx-1).valid
  //   } else {
  //     // the following code creates an anonymous module for each stage
  //     val stage_comb_module = Module(new Module {
  //       val intake = IO(Flipped(Valid(new ExtendedPipelineBundle(true))))
  //       val emit = IO(Valid(new ExtendedPipelineBundle(true)))
  //       val transform_function =
  //         stage_functions(idx).getOrElse((x: ExtendedPipelineBundle) =>
  //           identity(x)
  //         )
  //       emit.bits := transform_function(intake.bits)
  //       emit.valid := intake.valid
  //     })
  //     stage_comb_module.suggestName(s"stage_comb_module_${idx}")
  //     stage_comb_module.intake := stage_registers(idx - 1)
  //     stage_registers(idx) := stage_comb_module.emit
  //   }
  // }
  val stage_modules: Seq[
    SkidBufferStageModule[ExtendedPipelineBundle, ExtendedPipelineBundle]
  ] = stage_functions.toSeq.zipWithIndex.map { case (optF, idx) =>
    val stage = optF match {
      case Some(f) =>
        Module(SkidBufferStage(new ExtendedPipelineBundle(true), f))
      case None => Module(SkidBufferStage(new ExtendedPipelineBundle(true)))
    }
    stage.suggestName(s"stage_${idx}")
    stage
  }

  val _last_stage_emit_port = stage_modules.foldLeft(
    WireDefault(0.U.asTypeOf(Decoupled(new ExtendedPipelineBundle(true))))
  ) { case (w, s) =>
    s.intake :<>= w
    s.emit
  }
  _last_stage_emit_port.ready := true.B

  // now that all stages are chained up, overwrite the first few stages
  // stage 1 is deprecated, since we have skid buffers now
  // stage 2 converts FN to RecFN, so need a more generic SkidBufferStage
  val stage_2_actual_module = Module(
    GenerializedSkidBufferStage(
      new CombinedRayBoxTriangleBundle(false),
      new ExtendedPipelineBundle(true),
      { (input: CombinedRayBoxTriangleBundle) =>
        val output = WireDefault(0.U.asTypeOf(new ExtendedPipelineBundle(true)))
        output.isTriangleOp := input.isTriangleOp
        output.ray := RayConvertFNtoRecFN(input.ray)
        output.triangle := TriangleConvertFNtoRecFN(input.triangle)
        (output.aabb zip input.aabb).map { case (reg_o, reg_i) =>
          reg_o := AABBConvertFNtoRecFN(reg_i)
        }
        output
      }
    )
  ).suggestName(s"stage_2_actual")

  stage_2_actual_module.intake :<>= in
  stage_modules(3).intake :<>= stage_2_actual_module.emit

  // zero-th element is useless
  // stage_registers(0) := 0.U.asTypeOf(stage_registers(0))

  // // first element is useless too, because the first stage is "register input"
  // // stage, the values are still non-recorded float. See
  // // `stage_registers_1_actual` instead.
  // stage_registers(1) := 0.U.asTypeOf(stage_registers(0))

  // // override the driver logic for stage_registers 1 and 2
  // val stage_registers_1_actual = Reg(
  //   Valid(new CombinedRayBoxTriangleBundle(false))
  // )
  // stage_registers_1_actual.valid := in.fire
  // stage_registers_1_actual.bits := in.bits

  // stage_registers(2).valid := stage_registers_1_actual.valid
  // stage_registers(
  //   2
  // ).bits.isTriangleOp := stage_registers_1_actual.bits.isTriangleOp
  // stage_registers(2).bits.ray := RayConvertFNtoRecFN(
  //   stage_registers_1_actual.bits.ray
  // )
  // (stage_registers(2).bits.aabb zip stage_registers_1_actual.bits.aabb).map {
  //   case (reg_2, reg_1) =>
  //     reg_2 := AABBConvertFNtoRecFN(reg_1)
  // }
  // stage_registers(2).bits.triangle := TriangleConvertFNtoRecFN(
  //   stage_registers_1_actual.bits.triangle
  // )

  ////////////////////////
  // TAP OUTPUT FROM LAST MEANINGFUL STAGE'S REGISTER
  /////////////////////////

  // out.valid := stage_registers(10).valid
  // out.bits.isTriangleOp := stage_registers(10).bits.isTriangleOp
  // out.bits.tmin_out := stage_registers(10).bits.tmin.map(fNFromRecFN(8, 24, _))
  // out.bits.isIntersect := stage_registers(10).bits.isIntersect
  // out.bits.boxIndex := stage_registers(10).bits.boxIndex
  // out.bits.t_denom := fNFromRecFN(8, 24, stage_registers(10).bits.t_denom)
  // out.bits.t_num := fNFromRecFN(8, 24, stage_registers(10).bits.t_num)
  // out.bits.triangle_hit := stage_registers(10).bits.triangle_hit
  val output_stage = Module(
    GenerializedSkidBufferStage(
      new ExtendedPipelineBundle(true),
      new UnifiedDatapathOutput(false),
      { (input: ExtendedPipelineBundle) =>
        val output = Wire(new UnifiedDatapathOutput(false))
        output.isTriangleOp := input.isTriangleOp
        output.tmin_out := input.tmin.map(fNFromRecFN(8, 24, _))
        output.isIntersect := input.isIntersect
        output.boxIndex := input.boxIndex
        output.t_denom := fNFromRecFN(8, 24, input.t_denom)
        output.t_num := fNFromRecFN(8, 24, input.t_num)
        output.triangle_hit := input.triangle_hit
        output
      }
    )
  ).suggestName("stage_for_output")
  output_stage.intake :<>= stage_modules(10).emit
  out :<>= output_stage.emit
}
