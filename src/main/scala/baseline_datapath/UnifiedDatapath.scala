package baseline_datapath

import chisel3._
import hardfloat._
import chisel3.experimental.VecLiterals._ // for VecLit
import chisel3.util._
import chisel3.experimental.dataview._

case class RaytracerParams(
    // What format the FP numbers in IO is.
    // 32-bit IEEE-754 compliant float if false, 33-bit recorded format if true
    io_recorded_float: Boolean = false,

    // No euclidean support if None, else supports processing X dimensions per cycle given Some(X)
    support_euclidean: Option[Int] = None
)

object DatapathConstants {
  def _zero_RecFN = recFNFromFN(8, 24, 0x0.U)
  def _neg_1_0_RecFN = recFNFromFN(8, 24, 0xbf800000.S)
  def _pos_1_0_RecFN = recFNFromFN(8, 24, 0x3f800000.S)
  // 0x7f800000 is positive inf in 32-bit float
  def _positive_inf_RecFN = recFNFromFN(8, 24, 0x7f800000.S)
}

class UnifiedDatapath(p: RaytracerParams) extends Module {
  import DatapathConstants._

  // For now, we don't support recorded-format Input
  assert(p.io_recorded_float == false)

  // input is guaranteed to be registered by this module
  val in = IO(
    Flipped(
      Decoupled(
        new EnhancedInputBundle(recorded_float = false, element_count = 16)
      )
    )
  )

  // output is also registered by the last stage's output buffer
  val out = IO(
    Decoupled(new UnifiedDatapathOutput(recorded_float = false))
  )

  // A cycle counter, that we can use when debugging the module
  // It shall disappear in synthesis, so no PPA impact (provided we don't expose
  // _time when instantiating the top-level module)
  val (_time, _) = Counter(true.B, (1 << 31) - 1)
  dontTouch(_time)

  /////////////////
  // SOME CONSTANTS
  /////////////
  val _max_stage_count = 12;
  val _box_plurality =
    in.bits.aabb.length // this is a variable known at Chisel elaboration time
  val _rounding_rule = consts.round_near_even
  val _tininess_rule = consts.tininess_beforeRounding

  /////////////////
  // STAGE BEHAVIOR
  //////////////////

  // Stage dataflow (how to transform input to output) is decoupled from stage control
  // (whether to block for a cycle).

  // Below, the dataflow logic is specified. The stage control is implemented by
  // GeneralizedSkidBufferStage.

  // Each element of this array is an Option, that wraps a function object which
  // maps an input ExtendedPipelineBundle to an output ExtendedPipelineBundle
  val stage_functions = collection.mutable.ArrayBuffer.fill[Option[
    ExtendedPipelineBundle => ExtendedPipelineBundle
  ]](_max_stage_count)(None)

  // Define the functions!
  // stage 0 does not exist
  // stage 1 also doesn't exist, but kept for consistenty
  // stage 2 converts float32 to float33 (specified separately at the bottom, because the
  // function signature is different)

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
      fu.io.detectTininess := _tininess_rule
      fu.io.roundingMode := _rounding_rule
      fu.io.subOp := false.B
      fu
    }

    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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
            _1 := fu.io.out
          // TODO: handle exception flags!
        }
      }
      is(UnifiedDatapathOpCode.OpQuadbox) {
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
            _1 := fu.io.out
          // TODO: handle exception flags!
        }

      }
    }

    emit
  })

  // stage 4 performs 24 muls for ray-box to find out the time intersection
  // intervals, or 9 mul for ray-triangle to perform the multiplication step of shearing and scaling of
  // triangle vertices. Mul units are used for both.
  stage_functions(4) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    val fu_list: List[MulRecFN] = List.fill(24) {
      val fu = Module(new MulRecFN(8, 24))
      fu.io.a := 0.U
      fu.io.b := 0.U
      fu.io.detectTininess := _tininess_rule
      fu.io.roundingMode := _rounding_rule
      fu
    }

    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
        // references to wires from the intake
        val kx = intake.ray.kx
        val ky = intake.ray.ky
        val kz = intake.ray.kz

        val _dest = Seq(
          emit.A.x, // (-ray.shear.x) * A.at(kz)
          emit.A.y, // (-ray.shear.y) * A.at(kz)
          emit.A.z, // ray.shear.z * A.at(kz)
          emit.B.x, // (-ray.shear.x) * B.at(kz)
          emit.B.y, // (-ray.shear.y) * B.at(kz)
          emit.B.z, // ray.shear.z * B.at(kz)
          emit.C.x, // (-ray.shear.x) * C.at(kz)
          emit.C.y, // (-ray.shear.y) * C.at(kz)
          emit.C.z // ray.shear.z * C.at(kz)
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

        // _dest = _src1 * _src2
        (_src1 zip _src2 zip _dest zip fu_list).map {
          case (((_1, _2), _3), fu) =>
            fu.io.a := _1
            fu.io.b := _2
            _3 := fu.io.out
          // handle exception flags!
        }

      }
      is(UnifiedDatapathOpCode.OpQuadbox) {
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

        (_src1 zip _src2 zip _dest zip fu_list) foreach {
          case (((_1, _2), _3), fu) =>
            fu.io.a := _1
            fu.io.b := _2

            _3 := fu.io.out
          // TODO: handle exception flags!
        }

      }
    }

    emit
  })

  // stage 5 performs 6 adds for ray-triangle to perform the addition step of
  // shearing and scaling of triangle vertices.
  stage_functions(5) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake

    val fu_list: List[AddRecFN] = List.fill(8) {
      val fu = Module(new AddRecFN(8, 24))
      fu.io.a := 0.U
      fu.io.b := 0.U
      fu.io.detectTininess := _tininess_rule
      fu.io.roundingMode := _rounding_rule
      fu.io.subOp := false.B
      fu
    }

    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
        // the z-components don't need to add anything. In fact, the following
        // 3 lines are redundant since we already let emit:=intake
        emit.A.z := intake.A.z
        emit.B.z := intake.B.z
        emit.C.z := intake.C.z

        // reference to intake wires
        val kx = intake.ray.kx
        val ky = intake.ray.ky
        val kz = intake.ray.kz

        val _dest = Seq(
          emit.A.x, // A.at(kx) + (-ray.shear.x) * A.at(kz)
          emit.A.y, // A.at(ky) + (-ray.shear.y) * A.at(kz)
          emit.B.x, // B.at(kx) + (-ray.shear.x) * B.at(kz)
          emit.B.y, // B.at(ky) + (-ray.shear.y) * B.at(kz)
          emit.C.x, // C.at(kx) + (-ray.shear.x) * C.at(kz)
          emit.C.y // C.at(ky) + (-ray.shear.y) * C.at(kz)
        )

        val _src1 = Seq(
          intake.triangle.A.at(kx),
          intake.triangle.A.at(ky),
          intake.triangle.B.at(kx),
          intake.triangle.B.at(ky),
          intake.triangle.C.at(kx),
          intake.triangle.C.at(ky)
        )

        val _src2 = Seq(
          intake.A.x,
          intake.A.y,
          intake.B.x,
          intake.B.y,
          intake.C.x,
          intake.C.y
        )

        (_src1 zip _src2 zip _dest zip fu_list).map {
          case (((_1, _2), _3), fu) =>
            fu.io.a := _1
            fu.io.b := _2
            _3 := fu.io.out
            fu.io.subOp := false.B
        }
      }
      is(UnifiedDatapathOpCode.OpQuadbox) {
        // nothing
      }
    }

    emit
  })

  // stage 6 performs 6 muls for ray-triangle test to figure out the minuends and
  // subtrahends of U, V, W
  stage_functions(6) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake
    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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
      }
      is(UnifiedDatapathOpCode.OpQuadbox) {}
    }

    emit
  })

  // stage 7 performs 3 adds for ray-triangle test to find the value of U, V, W
  stage_functions(7) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake
    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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

      }
      is(UnifiedDatapathOpCode.OpQuadbox) {}
    }

    emit
  })

  // stage 8 calculates U*Az, V*Bz and W*Cz for ray-triangle test
  stage_functions(8) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake
    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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
      }
      is(UnifiedDatapathOpCode.OpQuadbox) {}
    }

    emit
  })

  // stage 9 does two adds for ray-triangle test to find out the partial sum for
  // t_denom and t_num
  stage_functions(9) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake
    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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

      }
      is(UnifiedDatapathOpCode.OpQuadbox) {}
    }

    emit
  })

  // stage 10 performs two adds for ray-triangle test, to complete the summation
  // of t_denom and t_num
  stage_functions(10) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake
    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
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

      }
      is(UnifiedDatapathOpCode.OpQuadbox) {}
    }

    emit
  })

  // stage 11 performs 12 CAS, 10 quad-sorts and 4 comparisons for ray-box
  // intersection to figure out intersecting boxes and sort them; or 5
  // comparisons for ray-triangle test to determine validity of intersection
  stage_functions(11) = Some({ intake =>
    val emit = Wire(new ExtendedPipelineBundle(true))
    emit := intake


    val comparator_fu_list = Seq.fill(5){
      val fu = Module(new CompareRecFN(8, 24))
      fu.io.a := 0.U 
      fu.io.b := 0.U 
      fu.io.signaling := true.B
      fu
    }

    switch(intake.opcode) {
      is(UnifiedDatapathOpCode.OpTriangle) {
        val _src1 = Seq(
          intake.U,
          intake.V,
          intake.W,
          intake.t_denom,
          intake.t_num
        )
        val _src2 = Seq.fill(5)(_zero_RecFN)

        // a reference to the comparators' list
        val _fu = comparator_fu_list

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

      }
      is(UnifiedDatapathOpCode.OpQuadbox) {
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

          // a reference to one of the comparators
          val comp_tmin_tmax = comparator_fu_list(box_idx)
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
        emit.boxIndex := quad_sort_for_box_index.io.sorted_indices.reverse

        val quad_sort_for_t = Module(new QuadSortRecFN)
        quad_sort_for_t.io.in := tmin_but_substitute_non_intersect_with_PInf
        emit.tmin := quad_sort_for_t.io.out.reverse

        emit.isIntersect.zip(emit.boxIndex).foreach { case (b, idx) =>
          b := isIntersect_intermediate(idx)
        }

      }
    }

    emit
  })

  //////////////////
  // STAGE REGISTERS
  ///////////////////

  // Instantiate the skid-capable pipeline stages and wrap the dataflow logic
  // specified above, or wrap the identity function by default.

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

  // Chain up the stages and (literally) tie the loose end.
  // We feed all-zero to stage 0. It's okay, because we will be overriding
  // the connections by routing the module input to stage_2_actual, and route
  // stage_2_actual to stage 3
  val _last_stage_emit_port = stage_modules.foldLeft(
    WireDefault(0.U.asTypeOf(Decoupled(new ExtendedPipelineBundle(true))))
  ) { case (w, s) =>
    s.intake :<>= w
    s.emit
  }
  _last_stage_emit_port.ready := true.B

  // now that all stages are chained up, overwrite the first few stages
  // stage 1 is deprecated, since we have skid buffers now
  // stage 2 converts FN to RecFN, so we need a more generic SkidBufferStage
  val stage_2_actual_module = Module(
    GenerializedSkidBufferStage(
      // new CombinedRayBoxTriangleBundle(false),
      new EnhancedInputBundle(false, 16),
      new ExtendedPipelineBundle(true),
      { (input: EnhancedInputBundle) =>
        val output = WireDefault(0.U.asTypeOf(new ExtendedPipelineBundle(true)))
        output.opcode := input.opcode
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

  ////////////////////////
  // TAP OUTPUT FROM LAST MEANINGFUL STAGE'S OUTPUT BUFFER
  /////////////////////////

  // the output stage also contains special logic: to convert RecFN to FN
  val output_stage = Module(
    GenerializedSkidBufferStage(
      new ExtendedPipelineBundle(true),
      new UnifiedDatapathOutput(false),
      { (input: ExtendedPipelineBundle) =>
        val output = Wire(new UnifiedDatapathOutput(false))
        output.opcode := input.opcode
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

  output_stage.intake :<>= stage_modules(11).emit
  out :<>= output_stage.emit
}
