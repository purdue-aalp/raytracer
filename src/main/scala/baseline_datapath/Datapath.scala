package baseline_datapath

import chisel3._
import hardfloat._
import chisel3.experimental.VecLiterals._ // for VecLit
import chisel3.util._

class Datapath extends Module {
  val in = IO(Flipped(Decoupled(new RayBoxPair(recorded_float = false))))

  val out = IO(Decoupled(new Bundle {
    val tmin_out = Bits(32.W)
    val isIntersect = Bool()
  }))

  // SOME CONSTANTS
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
    val convert_neg_one_int_to_neg_one_rec_float = Module(new INToRecFN(32, 8, 24))
    convert_neg_one_int_to_neg_one_rec_float.io.signedIn := true.B
    convert_neg_one_int_to_neg_one_rec_float.io.in := -1.S(32.W).asUInt
    convert_neg_one_int_to_neg_one_rec_float.io.roundingMode := _rounding_rule
    convert_neg_one_int_to_neg_one_rec_float.io.detectTininess := _tininess_rule
    val out_val = convert_neg_one_int_to_neg_one_rec_float.io.out
    out_val
  }

  // A cycle counter, that we can use when debugging the module
  // It shall disappear in synthesis, so no PPA impact
  val (_time, _) = Counter(true.B, (1<<31)-1)
  dontTouch(_time)

  // always ready to accept jobs
  in.ready := WireDefault(true.B)

  // A shift register for the ray, boxes, and triangle inputs of each cycle.
  // We will chain-up everything, and force feed zero bits to the input 
  // However later in the code, we will overwrite "index 2" of this shift
  // register with the post format-conversion values of the ray, boxes and
  // triangle. We will then overwrite "index 3" of the boxes and triangles with
  // their coordinate-translated values.
  // Thanks to the last-connect semantics for this.
  val _shift_reg_length = 15
  val geometries_shift_reg = Reg(Vec(_shift_reg_length, new RayBoxPair(recorded_float = true)))
  geometries_shift_reg(0) := 0.U.asTypeOf(geometries_shift_reg(0)) // force-feed zeros to input
  for(idx <- 1 until _shift_reg_length){
    geometries_shift_reg(idx) := geometries_shift_reg(idx-1)
  }

  //
  // STAGE 1: REGISTER INPUTS
  //
  val ray_1 = RegNext(in.bits.ray)
  val aabb_1 = RegNext(in.bits.aabb)

  //
  // STAGE 2: CONVERT FLOAT FORMAT: 32->33
  //
  geometries_shift_reg(2).ray := RayConvertFNtoRecFN(ray_1)
  geometries_shift_reg(2).aabb := AABBConvertFNtoRecFN(aabb_1)

  //
  // STAGE 3:  translate box relative to ray origin
  //
  val adder_exceptions = Seq.fill(6)(Wire(UInt(5.W)))

  // the following implements the C-code:
  /*
    child.x_min = child.x_min - ray.origin.x;
    child.y_min = child.y_min - ray.origin.y;
    child.z_min = child.z_min - ray.origin.z;
    child.x_max = child.x_max - ray.origin.x;
    child.y_max = child.y_max - ray.origin.y;
    child.z_max = child.z_max - ray.origin.z;
   */
  {
    val _dest = Seq(
      geometries_shift_reg(3).aabb.x_min,
      geometries_shift_reg(3).aabb.y_min,
      geometries_shift_reg(3).aabb.z_min,
      geometries_shift_reg(3).aabb.x_max,
      geometries_shift_reg(3).aabb.y_max,
      geometries_shift_reg(3).aabb.z_max
    )
    val _src1 = Seq(
      geometries_shift_reg(2).aabb.x_min,
      geometries_shift_reg(2).aabb.y_min,
      geometries_shift_reg(2).aabb.z_min,
      geometries_shift_reg(2).aabb.x_max,
      geometries_shift_reg(2).aabb.y_max,
      geometries_shift_reg(2).aabb.z_max
    )
    val _src2 = Seq(
      geometries_shift_reg(2).ray.origin.x,
      geometries_shift_reg(2).ray.origin.y,
      geometries_shift_reg(2).ray.origin.z,
      geometries_shift_reg(2).ray.origin.x,
      geometries_shift_reg(2).ray.origin.y,
      geometries_shift_reg(2).ray.origin.z
    )
    (_dest zip _src1 zip _src2 zip adder_exceptions) foreach {
      case (((_1, _2), _3), _4) =>
        val fu = Module(new AddRecFN(8, 24))
        fu.io.subOp := true.B
        fu.io.a := _2
        fu.io.b := _3
        fu.io.roundingMode := _rounding_rule
        fu.io.detectTininess := _tininess_rule
        _1 := fu.io.out
        _4 := fu.io.exceptionFlags
      // TODO: handle exception flags!
    }
  }

  //
  // STAGE 4: Time intersection interval calculations for each axis plane
  //
  val tp_min_4 = Reg(new Float3(recorded_float = true))
  val tp_max_4 = Reg(new Float3(recorded_float = true))

  // the following implements the C-code
  /*
    float tp_min_x = child.x_min * ray.inv.x;
    float tp_min_y = child.y_min * ray.inv.y;
    float tp_min_z = child.z_min * ray.inv.z;
    float tp_max_x = child.x_max * ray.inv.x;
    float tp_max_y = child.y_max * ray.inv.y;
    float tp_max_z = child.z_max * ray.inv.z;
   */
  {
    val _dest = Seq(
      tp_min_4.x,
      tp_min_4.y,
      tp_min_4.z,
      tp_max_4.x,
      tp_max_4.y,
      tp_max_4.z
    )
    val _src1 = Seq(
      geometries_shift_reg(3).aabb.x_min,
      geometries_shift_reg(3).aabb.y_min,
      geometries_shift_reg(3).aabb.z_min,
      geometries_shift_reg(3).aabb.x_max,
      geometries_shift_reg(3).aabb.y_max,
      geometries_shift_reg(3).aabb.z_max
    )
    val _src2 = Seq(
      geometries_shift_reg(3).ray.inv.x,
      geometries_shift_reg(3).ray.inv.y,
      geometries_shift_reg(3).ray.inv.z,
      geometries_shift_reg(3).ray.inv.x,
      geometries_shift_reg(3).ray.inv.y,
      geometries_shift_reg(3).ray.inv.z
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

  //
  // STAGE 5: SORTING
  //
  val tmin_3d = Wire(new Float3(recorded_float = true))
  val tmax_3d = Wire(new Float3(recorded_float = true))
  val isIntersect_5 = Reg(Bool())
  val tmin_out_rec_5 = Reg(Bits(33.W))

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

  flip_intervals_if_dir_is_neg(
    tmin_3d.x,
    tmax_3d.x,
    geometries_shift_reg(4).ray.dir.x,
    _zero_RecFN,
    tp_min_4.x,
    tp_max_4.x,
    true
  )
  flip_intervals_if_dir_is_neg(
    tmin_3d.y,
    tmax_3d.y,
    geometries_shift_reg(4).ray.dir.y,
    _zero_RecFN,
    tp_min_4.y,
    tp_max_4.y,
    true
  )
  flip_intervals_if_dir_is_neg(
    tmin_3d.z,
    tmax_3d.z,
    geometries_shift_reg(4).ray.dir.z,
    _zero_RecFN,
    tp_min_4.z,
    tp_max_4.z,
    true
  )

  // find the largest among all three dimensions of tmin_3d and 0.0f
  // find the smallest among all three dimensions of tmax_3d and 0.0f
  val tmin = Wire(Bits(33.W))
  val tmax = Wire(Bits(33.W))

  val quad_sort_for_tmin = Module(new QuadSortRecFN())
  quad_sort_for_tmin.io.in(0) := tmin_3d.x
  quad_sort_for_tmin.io.in(1) := tmin_3d.y
  quad_sort_for_tmin.io.in(2) := tmin_3d.z
  quad_sort_for_tmin.io.in(3) := _zero_RecFN
  tmin := quad_sort_for_tmin.io.largest

  val quad_sort_for_tmax = Module(new QuadSortRecFN())
  quad_sort_for_tmax.io.in(0) := tmax_3d.x
  quad_sort_for_tmax.io.in(1) := tmax_3d.y
  quad_sort_for_tmax.io.in(2) := tmax_3d.z
  quad_sort_for_tmax.io.in(3) := geometries_shift_reg(4).ray.extent
  tmax := quad_sort_for_tmax.io.smallest

  // if there's overlap between [tmin, inf) and (-inf, tmax], we say ray-box
  // intersection happens
  val comp_tmin_tmax = Module(new CompareRecFN(8, 24))
  comp_tmin_tmax.io.a := tmin
  comp_tmin_tmax.io.b := tmax
  comp_tmin_tmax.io.signaling := true.B

  isIntersect_5 := comp_tmin_tmax.io.lt
  tmin_out_rec_5 := tmin

  //
  // STAGE 6: 33->32 CONVERSION AND OUTPUT DRIVING
  //

  val isIntersect_6 = RegNext(isIntersect_5)
  val tmin_out_6 = RegNext(
    Mux(
      isIntersect_5,
      fNFromRecFN(8, 24, tmin_out_rec_5),
      fNFromRecFN(8, 24, _neg_1_0_RecFN)
    )
  )

  out.bits.isIntersect := isIntersect_6
  out.bits.tmin_out := tmin_out_6
  out.valid := ShiftRegister(in.valid, 6)

}
