package baseline_datapath

import chisel3._
import hardfloat.{recFNFromFN, fNFromRecFN}

object UnifiedDatapathOpCode extends ChiselEnum {
  /// Ray -Triangle test
  val OpTriangle = Value

  /// Ray-Box tests for four boxes
  val OpQuadbox = Value

  /// Calculate the sum-of-squares of the element-wise difference bewteen two
  /// FP32 vectors
  val OpEuclidean = Value
}

class Float3(recorded_float: Boolean = false) extends Bundle {

  /** Defines a point in three-dimensional space. With "recorded_float" being
    * false, the expected format is IEEE-754 32-bit float With "recorded_float"
    * being true, the expected format is (1 bit sign + 9 bit exponent + 23 bit
    * fraction). This is the 33-bit long "recorded float" format used by the
    * HardFloat library, that corresponds to the 32-bit standard float.
    */
  val bit_width = if (recorded_float) 33.W else 32.W

  val x = Bits(bit_width)
  val y = Bits(bit_width)
  val z = Bits(bit_width)

  def isRecordedFloat(): Boolean = { recorded_float }
  def at(idx: UInt): Bits = {
    val output = Wire(Bits(bit_width))
    when(idx === 0.U) {
      output := x
    }.elsewhen(idx === 1.U) {
      output := y
    }.otherwise {
      output := z
    }
    output
  }
}

class Ray(recorded_float: Boolean = false) extends Bundle {

  /** A ray in three-dimensional space.
    */
  val origin = new Float3(recorded_float)
  val dir = new Float3(recorded_float)
  val inv = new Float3(recorded_float)

  val _bit_width = if (recorded_float) 33.W else 32.W
  val extent = Bits(_bit_width)

  val kx = UInt(2.W)
  val ky = UInt(2.W)
  val kz = UInt(2.W)
  val shear = new Float3(recorded_float)

  def isRecordedFloat(): Boolean = { recorded_float }
}

class AABB(recorded_float: Boolean = false) extends Bundle {

  /** Defines an Axis-Aligned Bounding Box (AABB) in three-dimensional space.
    * All values are either IEEE-754 floats (!recorded_float) or 33-bit recorded
    * floats .
    */

  val bit_width = if (recorded_float) 33.W else 32.W

  val x_min = Bits(bit_width)
  val y_min = Bits(bit_width)
  val z_min = Bits(bit_width)

  val x_max = Bits(bit_width)
  val y_max = Bits(bit_width)
  val z_max = Bits(bit_width)

  def isRecordedFloat(): Boolean = { recorded_float }
}

class Triangle(recorded_float: Boolean = false) extends Bundle {
  val A = new Float3(recorded_float)
  val B = new Float3(recorded_float)
  val C = new Float3(recorded_float)

  def isRecordedFloat(): Boolean = { recorded_float }
}

class RayBoxPair(recorded_float: Boolean = false) extends Bundle {
  val ray = new Ray(recorded_float)
  val aabb = new AABB(recorded_float)
}

class CombinedRayBoxTriangleBundle(val recorded_float: Boolean = false)
    extends Bundle {
  val _bit_width = if (recorded_float) 33.W else 32.W

  val ray = new Ray(recorded_float)

  val aabb = Vec(4, new AABB(recorded_float))
  val triangle = new Triangle(recorded_float)

  // if true, perform ray-triangle intersection test
  // if false, perform ray-box interesction tests
  val opcode = UnifiedDatapathOpCode()
}

/// Element count is how many elements to calculate euclidean distance for, in
/// each cycle.
class EnhancedInputBundle(
    recorded_float: Boolean = false,
    val element_count: Int = 16
) extends CombinedRayBoxTriangleBundle(recorded_float) {

  // for euclidean distance calculation
  val euclidean_mask = Bits(element_count.W)
  val euclidean_reset_accum = Bool()
  val euclidean_a = Vec(element_count, Bits(_bit_width))
  val euclidean_b = Vec(element_count, Bits(_bit_width))
}

class UnifiedDatapathOutput(recorded_float: Boolean = false) extends Bundle {
  val _bit_width = if (recorded_float) 33.W else 32.W

  /** Common
    */
  val opcode = UnifiedDatapathOpCode()

  /** For Ray-Box intersection
    */
  // from index-0 to index-3, intersecting boxes goes before non-intersecting
  // boxes, nearer hits go before further hits.
  // The tmin_out for non-intersects are PositiveInfinity
  val tmin_out = Vec(4, Bits(32.W))
  val isIntersect = Vec(4, Bool())
  val boxIndex = Vec(4, UInt(2.W))

  /** For Ray-Triangle intersection
    */
  val t_num = Bits(_bit_width)
  val t_denom = Bits(_bit_width)
  val triangle_hit = Bool()
}

// Nothing more than this bundle needs to be passed between pipeline stages of
// the Unified raytracer datapath. Obviously, not all fields will be used in
// every stage. I entrust the several levels of compilers (FIRRTL, Verilator,
// proprietary RTL synthesizer) to remove unused signals automatically.
class ExtendedPipelineBundle(recorded_float: Boolean)
    extends CombinedRayBoxTriangleBundle(recorded_float) {

  // common and basic: defined in base trait CombinedRayBoxTriangleBundle
  // val ray = new Ray(recorded_float)
  // val aabb = Vec(4, new AABB(recorded_float))
  // val triangle = new Triangle(recorded_float)
  // val isTriangleOp = Bool()

  // for ray-box intersection tests
  val t_min = Vec(4, new Float3(recorded_float))
  val t_max = Vec(4, new Float3(recorded_float))
  val tmin = Vec(4, Bits(_bit_width))
  val tmax = Vec(4, Bits(_bit_width))
  val isIntersect = Vec(4, Bool())
  val boxIndex = Vec(4, UInt(2.W))

  // for ray-triangle intersection tests
  val A = new Float3(recorded_float)
  val B = new Float3(recorded_float)
  val C = new Float3(recorded_float)
  val U = Bits(_bit_width)
  val V = Bits(_bit_width)
  val W = Bits(_bit_width)
  val U_subtrahend = Bits(_bit_width)
  val V_subtrahend = Bits(_bit_width)
  val W_subtrahend = Bits(_bit_width)
  val U_Az = Bits(_bit_width)
  val V_Bz = Bits(_bit_width)
  val W_Cz = Bits(_bit_width)
  val t_denom = Bits(_bit_width)
  val t_num = Bits(_bit_width)
  val triangle_hit = Bool()
}

// Conversion circuits between 32-bit IEEE float and 33-bit recorded float

object Float3ConvertFNtoRecFN {
  def apply(in: Float3): Float3 = {
    if (in.isRecordedFloat()) {
      throw new Exception(
        "input must be in IEEE-754 format before it can be converted to recorded format"
      )
    }
    val out = Wire(new Float3(recorded_float = true))
    out.x := recFNFromFN(8, 24, in.x)
    out.y := recFNFromFN(8, 24, in.y)
    out.z := recFNFromFN(8, 24, in.z)
    out
  }
}

object Float3ConvertRecFNtoFN {
  def apply(in: Float3): Float3 = {
    if (!in.isRecordedFloat()) {
      throw new Exception(
        "input must be in recorded format before it can be converted to IEEE-754 format"
      )
    }
    val out = Wire(new Float3(recorded_float = false))
    out.x := fNFromRecFN(8, 24, in.x)
    out.y := fNFromRecFN(8, 24, in.y)
    out.z := fNFromRecFN(8, 24, in.z)
    out
  }
}

object RayConvertFNtoRecFN {
  def apply(in: Ray): Ray = {
    if (in.isRecordedFloat()) {
      throw new Exception(
        "Ray must be in IEEE-754 format before it can be converted to recorded format"
      )
    }

    val out = Wire(new Ray(recorded_float = true))
    out.origin.x := recFNFromFN(8, 24, in.origin.x)
    out.origin.y := recFNFromFN(8, 24, in.origin.y)
    out.origin.z := recFNFromFN(8, 24, in.origin.z)
    out.dir.x := recFNFromFN(8, 24, in.dir.x)
    out.dir.y := recFNFromFN(8, 24, in.dir.y)
    out.dir.z := recFNFromFN(8, 24, in.dir.z)
    out.inv.x := recFNFromFN(8, 24, in.inv.x)
    out.inv.y := recFNFromFN(8, 24, in.inv.y)
    out.inv.z := recFNFromFN(8, 24, in.inv.z)
    out.extent := recFNFromFN(8, 24, in.extent)
    out.kx := in.kx
    out.ky := in.ky
    out.kz := in.kz
    out.shear.x := recFNFromFN(8, 24, in.shear.x)
    out.shear.y := recFNFromFN(8, 24, in.shear.y)
    out.shear.z := recFNFromFN(8, 24, in.shear.z)

    out
  }
}

object RayConvertRecFNtoFN {
  def apply(in: Ray): Ray = {
    if (in.isRecordedFloat() == false) {
      throw new Exception(
        "Ray must be in recorded format before it can be converted to IEEE-754 format"
      )
    }

    val out = Wire(new Ray(recorded_float = false))
    out.origin.x := fNFromRecFN(8, 24, in.origin.x)
    out.origin.y := fNFromRecFN(8, 24, in.origin.y)
    out.origin.z := fNFromRecFN(8, 24, in.origin.z)
    out.dir.x := fNFromRecFN(8, 24, in.dir.x)
    out.dir.y := fNFromRecFN(8, 24, in.dir.y)
    out.dir.z := fNFromRecFN(8, 24, in.dir.z)
    out.inv.x := fNFromRecFN(8, 24, in.inv.x)
    out.inv.y := fNFromRecFN(8, 24, in.inv.y)
    out.inv.z := fNFromRecFN(8, 24, in.inv.z)
    out.extent := fNFromRecFN(8, 24, in.extent)
    out.kx := in.kx
    out.ky := in.ky
    out.kz := in.kz
    out.shear.x := fNFromRecFN(8, 24, in.shear.x)
    out.shear.y := fNFromRecFN(8, 24, in.shear.y)
    out.shear.z := fNFromRecFN(8, 24, in.shear.z)

    out
  }
}

object AABBConvertFNtoRecFN {
  def apply(in: AABB): AABB = {
    if (in.isRecordedFloat()) {
      throw new Exception(
        "AABB must be in IEEE-754 format before it can be converted to recorded format"
      )
    }

    val out = Wire(new AABB(recorded_float = true))
    out.x_min := recFNFromFN(8, 24, in.x_min)
    out.y_min := recFNFromFN(8, 24, in.y_min)
    out.z_min := recFNFromFN(8, 24, in.z_min)

    out.x_max := recFNFromFN(8, 24, in.x_max)
    out.y_max := recFNFromFN(8, 24, in.y_max)
    out.z_max := recFNFromFN(8, 24, in.z_max)

    out
  }
}

object AABBConvertRecFNtoFN {
  def apply(in: AABB): AABB = {
    if (in.isRecordedFloat() == false) {
      throw new Exception(
        "AABB must be in recorded format before it can be converted to IEEE-754 format"
      )
    }

    val out = Wire(new AABB(recorded_float = false))
    out.x_min := fNFromRecFN(8, 24, in.x_min)
    out.y_min := fNFromRecFN(8, 24, in.y_min)
    out.z_min := fNFromRecFN(8, 24, in.z_min)

    out.x_max := fNFromRecFN(8, 24, in.x_max)
    out.y_max := fNFromRecFN(8, 24, in.y_max)
    out.z_max := fNFromRecFN(8, 24, in.z_max)

    out
  }
}

object TriangleConvertFNtoRecFN {
  def apply(in: Triangle): Triangle = {
    if (in.isRecordedFloat() == true) {
      throw new Exception(
        "AABB must be in recorded format before it can be converted to IEEE-754 format"
      )
    }

    val out = Wire(new Triangle(true))
    out.A := Float3ConvertFNtoRecFN(in.A)
    out.B := Float3ConvertFNtoRecFN(in.B)
    out.C := Float3ConvertFNtoRecFN(in.C)

    out
  }
}

object TriangleConvertRecFNtoFN {
  def apply(in: Triangle): Triangle = {
    if (in.isRecordedFloat() == false) {
      throw new Exception(
        "AABB must be in recorded format before it can be converted to IEEE-754 format"
      )
    }

    val out = Wire(new Triangle(false))
    out.A := Float3ConvertRecFNtoFN(in.A)
    out.B := Float3ConvertRecFNtoFN(in.B)
    out.C := Float3ConvertRecFNtoFN(in.C)

    out
  }
}
