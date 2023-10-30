package baseline_datapath.raytracer_gold

import chisel3._
import baseline_datapath._
import scala.language.implicitConversions
import java.nio.ByteBuffer

object floatToBits {

  /** Given a Scala float, returns an Int directly casted from the bits of the
    * float.
    *
    * Similar to C-code: int x; return y = *((float*)(&x));
    *
    * @param x
    *   A Float value
    * @return
    *   The integer cast of x.
    */
  def apply(x: Float)(implicit width: Int = 32): UInt = {
    val bb: ByteBuffer = ByteBuffer.allocate(8)
    bb.putInt(0)
    bb.putFloat(x)

    // bb now looks like [00][00][00][00][x3][x2][x1][x0]
    // The reason we allocate 8 bytes and pad the first four with zeros is to
    // prevent negative x values from triggering a "UInt cannot be negative"
    // error from Chisel.
    // y will get truncated anyway
    val y = bb.rewind().getLong()
    y.U(width.W)
  }
}

object bitsToFloat {
  def apply(x: UInt): Float = {
    assert(x.getWidth == 32)
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putInt(x.litValue.intValue)
    val y = bb.rewind().getFloat()
    y
  }
}

/** A bunch of implicit conversion methods between SW and HW data types
  */
object RaytracerTestHelper {

  // Whereas a RayBoxPair is expected, but see instead a tuple[SW_Ray, SW_Box],
  // this implicit conversion will be made.
  implicit def fromSWRayAndSWBoxToRayBoxPair(
      rb: (SW_Ray, SW_Box)
  ): RayBoxPair = {
    import chisel3.experimental.BundleLiterals._
    val (sw_ray, sw_box) = rb
    lazy val dummy_rmp = new RayBoxPair(false)
    dummy_rmp.Lit(
      _.ray.origin.x -> floatToBits(sw_ray.origin.x),
      _.ray.origin.y -> floatToBits(sw_ray.origin.y),
      _.ray.origin.z -> floatToBits(sw_ray.origin.z),
      _.ray.dir.x -> floatToBits(sw_ray.dir.x),
      _.ray.dir.y -> floatToBits(sw_ray.dir.y),
      _.ray.dir.z -> floatToBits(sw_ray.dir.z),
      _.ray.inv.x -> floatToBits(sw_ray.inv.x),
      _.ray.inv.y -> floatToBits(sw_ray.inv.y),
      _.ray.inv.z -> floatToBits(sw_ray.inv.z),
      _.ray.extent -> floatToBits(sw_ray.extent),
      _.ray.kx -> sw_ray.kx.U,
      _.ray.ky -> sw_ray.ky.U,
      _.ray.kz -> sw_ray.kz.U,
      _.ray.shear.x -> floatToBits(sw_ray.shear.x),
      _.ray.shear.y -> floatToBits(sw_ray.shear.y),
      _.ray.shear.z -> floatToBits(sw_ray.shear.z),
      _.aabb.x_min -> floatToBits(sw_box.x_min),
      _.aabb.x_max -> floatToBits(sw_box.x_max),
      _.aabb.y_min -> floatToBits(sw_box.y_min),
      _.aabb.y_max -> floatToBits(sw_box.y_max),
      _.aabb.z_min -> floatToBits(sw_box.z_min),
      _.aabb.z_max -> floatToBits(sw_box.z_max)
    )
  }

  implicit def fromSWRaySWBoxSeqToRayBoxPairSeq(
      rbseq: Seq[(SW_Ray, SW_Box)]
  ): Seq[RayBoxPair] = {
    rbseq.map(fromSWRayAndSWBoxToRayBoxPair(_))
  }

  implicit def fromSWCombinedDataToCombinedRayBoxTriangleBundle(
      rb: SW_CombinedData
  ): CombinedRayBoxTriangleBundle = {
    import chisel3.experimental.BundleLiterals._
    val sw_ray = rb.ray
    val sw_box = rb.boxes
    val sw_triangle = rb.triangle
    val op = rb.opcode
    assert(sw_box.length == 4)

    lazy val dummy_crbtb = new CombinedRayBoxTriangleBundle(false)
    lazy val dummy_aabb = new AABB(false)

    dummy_crbtb.Lit(
      _.opcode -> UnifiedDatapathOpCode(op.id.U),
      _.ray.origin.x -> floatToBits(sw_ray.origin.x),
      _.ray.origin.y -> floatToBits(sw_ray.origin.y),
      _.ray.origin.z -> floatToBits(sw_ray.origin.z),
      _.ray.dir.x -> floatToBits(sw_ray.dir.x),
      _.ray.dir.y -> floatToBits(sw_ray.dir.y),
      _.ray.dir.z -> floatToBits(sw_ray.dir.z),
      _.ray.inv.x -> floatToBits(sw_ray.inv.x),
      _.ray.inv.y -> floatToBits(sw_ray.inv.y),
      _.ray.inv.z -> floatToBits(sw_ray.inv.z),
      _.ray.extent -> floatToBits(sw_ray.extent),
      _.ray.kx -> sw_ray.kx.U,
      _.ray.ky -> sw_ray.ky.U,
      _.ray.kz -> sw_ray.kz.U,
      _.ray.shear.x -> floatToBits(sw_ray.shear.x),
      _.ray.shear.y -> floatToBits(sw_ray.shear.y),
      _.ray.shear.z -> floatToBits(sw_ray.shear.z),
      _.triangle.A.x -> floatToBits(sw_triangle.A.x),
      _.triangle.A.y -> floatToBits(sw_triangle.A.y),
      _.triangle.A.z -> floatToBits(sw_triangle.A.z),
      _.triangle.B.x -> floatToBits(sw_triangle.B.x),
      _.triangle.B.y -> floatToBits(sw_triangle.B.y),
      _.triangle.B.z -> floatToBits(sw_triangle.B.z),
      _.triangle.C.x -> floatToBits(sw_triangle.C.x),
      _.triangle.C.y -> floatToBits(sw_triangle.C.y),
      _.triangle.C.z -> floatToBits(sw_triangle.C.z),
      _.aabb(0) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(0).x_min),
          _.x_max -> floatToBits(sw_box(0).x_max),
          _.y_min -> floatToBits(sw_box(0).y_min),
          _.y_max -> floatToBits(sw_box(0).y_max),
          _.z_min -> floatToBits(sw_box(0).z_min),
          _.z_max -> floatToBits(sw_box(0).z_max)
        ),
      _.aabb(1) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(1).x_min),
          _.x_max -> floatToBits(sw_box(1).x_max),
          _.y_min -> floatToBits(sw_box(1).y_min),
          _.y_max -> floatToBits(sw_box(1).y_max),
          _.z_min -> floatToBits(sw_box(1).z_min),
          _.z_max -> floatToBits(sw_box(1).z_max)
        ),
      _.aabb(2) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(2).x_min),
          _.x_max -> floatToBits(sw_box(2).x_max),
          _.y_min -> floatToBits(sw_box(2).y_min),
          _.y_max -> floatToBits(sw_box(2).y_max),
          _.z_min -> floatToBits(sw_box(2).z_min),
          _.z_max -> floatToBits(sw_box(2).z_max)
        ),
      _.aabb(3) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(3).x_min),
          _.x_max -> floatToBits(sw_box(3).x_max),
          _.y_min -> floatToBits(sw_box(3).y_min),
          _.y_max -> floatToBits(sw_box(3).y_max),
          _.z_min -> floatToBits(sw_box(3).z_min),
          _.z_max -> floatToBits(sw_box(3).z_max)
        )
    )
  }

  implicit def fromSWCombinedDataSeqToCombinedRayBoxTriangleBundleSeq(
      rbseq: Seq[SW_CombinedData]
  ): Seq[CombinedRayBoxTriangleBundle] = {
    rbseq.map(fromSWCombinedDataToCombinedRayBoxTriangleBundle(_))
  }

  implicit def fromSWEnhancedCombinedDataToEnhancedInputBundle(
      sw_data: SW_EnhancedCombinedData
  ): EnhancedInputBundle = {
    import chisel3.experimental.BundleLiterals._
    import chisel3.experimental.VecLiterals._

    val sw_ray = sw_data.ray
    val sw_box = sw_data.boxes
    val sw_triangle = sw_data.triangle
    val op = sw_data.opcode
    val vec_a = sw_data.vector_a
    val vec_b = sw_data.vector_b
    assert(sw_box.length == 4)

    val vec_a_as_bits = vec_a.get_elements().map(floatToBits(_))
    val vec_b_as_bits = vec_b.get_elements().map(floatToBits(_))

    lazy val dummy_eib = new EnhancedInputBundle(false)
    lazy val dummy_aabb = new AABB(false)

    dummy_eib.Lit(
      _.opcode -> UnifiedDatapathOpCode(op.id.U),
      _.ray.origin.x -> floatToBits(sw_ray.origin.x),
      _.ray.origin.y -> floatToBits(sw_ray.origin.y),
      _.ray.origin.z -> floatToBits(sw_ray.origin.z),
      _.ray.dir.x -> floatToBits(sw_ray.dir.x),
      _.ray.dir.y -> floatToBits(sw_ray.dir.y),
      _.ray.dir.z -> floatToBits(sw_ray.dir.z),
      _.ray.inv.x -> floatToBits(sw_ray.inv.x),
      _.ray.inv.y -> floatToBits(sw_ray.inv.y),
      _.ray.inv.z -> floatToBits(sw_ray.inv.z),
      _.ray.extent -> floatToBits(sw_ray.extent),
      _.ray.kx -> sw_ray.kx.U,
      _.ray.ky -> sw_ray.ky.U,
      _.ray.kz -> sw_ray.kz.U,
      _.ray.shear.x -> floatToBits(sw_ray.shear.x),
      _.ray.shear.y -> floatToBits(sw_ray.shear.y),
      _.ray.shear.z -> floatToBits(sw_ray.shear.z),
      _.triangle.A.x -> floatToBits(sw_triangle.A.x),
      _.triangle.A.y -> floatToBits(sw_triangle.A.y),
      _.triangle.A.z -> floatToBits(sw_triangle.A.z),
      _.triangle.B.x -> floatToBits(sw_triangle.B.x),
      _.triangle.B.y -> floatToBits(sw_triangle.B.y),
      _.triangle.B.z -> floatToBits(sw_triangle.B.z),
      _.triangle.C.x -> floatToBits(sw_triangle.C.x),
      _.triangle.C.y -> floatToBits(sw_triangle.C.y),
      _.triangle.C.z -> floatToBits(sw_triangle.C.z),
      _.aabb(0) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(0).x_min),
          _.x_max -> floatToBits(sw_box(0).x_max),
          _.y_min -> floatToBits(sw_box(0).y_min),
          _.y_max -> floatToBits(sw_box(0).y_max),
          _.z_min -> floatToBits(sw_box(0).z_min),
          _.z_max -> floatToBits(sw_box(0).z_max)
        ),
      _.aabb(1) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(1).x_min),
          _.x_max -> floatToBits(sw_box(1).x_max),
          _.y_min -> floatToBits(sw_box(1).y_min),
          _.y_max -> floatToBits(sw_box(1).y_max),
          _.z_min -> floatToBits(sw_box(1).z_min),
          _.z_max -> floatToBits(sw_box(1).z_max)
        ),
      _.aabb(2) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(2).x_min),
          _.x_max -> floatToBits(sw_box(2).x_max),
          _.y_min -> floatToBits(sw_box(2).y_min),
          _.y_max -> floatToBits(sw_box(2).y_max),
          _.z_min -> floatToBits(sw_box(2).z_min),
          _.z_max -> floatToBits(sw_box(2).z_max)
        ),
      _.aabb(3) ->
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(3).x_min),
          _.x_max -> floatToBits(sw_box(3).x_max),
          _.y_min -> floatToBits(sw_box(3).y_min),
          _.y_max -> floatToBits(sw_box(3).y_max),
          _.z_min -> floatToBits(sw_box(3).z_min),
          _.z_max -> floatToBits(sw_box(3).z_max)
        ),
      _.euclidean_a -> Vec.Lit(vec_a_as_bits: _*),
      _.euclidean_b -> Vec.Lit(vec_b_as_bits: _*)
    )
  }

  implicit def fromSWEnhancedCombinedDataSeqToEnhancedInputBundleSeq(
      sw_data: Seq[SW_EnhancedCombinedData]
  ): Seq[EnhancedInputBundle] = {
    sw_data.map(fromSWEnhancedCombinedDataToEnhancedInputBundle(_))
  }

}
