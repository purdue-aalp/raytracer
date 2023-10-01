package baseline_datapath.raytracer_gold

import chisel3._
import scala.math._
import baseline_datapath._
import chiseltest._
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
case class float_3(
    val x: Float,
    val y: Float,
    val z: Float
){
  def at(n: Int): Float = n match {
    case 0 => x 
    case 1 => y 
    case 2 => z 
    case _ => throw new Exception("Float only has 0, 1, 2 three dimensions")
  }
}

class SW_Ray(
    val origin: float_3,
    val dir: float_3
) {
  def max_dim(): Int = {
    val x: Float = math.abs(dir.x)
    val y: Float = math.abs(dir.y)
    val z: Float = math.abs(dir.z)

    var maxInd = 0 
    var maxVal = x

    if(y > maxVal){
      maxVal = y
      maxInd = 1 
    }
    if(z > maxVal){
      maxInd = 2
    }

    maxInd
  }

  val extent: Float = SW_Ray.RAY_EXTENT.toFloat
  val inv: float_3 = {
    val _x = 1.0f / dir.x
    val _y = 1.0f / dir.y
    val _z = 1.0f / dir.z
    val _inv = float_3(_x, _y, _z)
    _inv
  }
  
  val (kx, ky, kz): (Int, Int, Int) = {
    var _kz = max_dim()
    var _kx = if(_kz+1==3){0}else{_kz+1}
    var _ky = if(_kx+1==3){0}else{_kx+1}
    if(dir.at(_kz) < 0.0f){
      val temp = _kx 
      _kx = _ky 
      _ky = temp
    }
    (_kx, _ky, _kz)
  }
  
  val shear = float_3(
    x = dir.at(kx) / dir.at(kz),
    y = dir.at(ky) / dir.at(kz),
    z = 1.0f / dir.at(kz)
  )
}

object SW_Ray {
  val SCENE_BOUNDS = 100000
  val RAY_EXTENT = 4 * SCENE_BOUNDS
  def apply(_ori: float_3, _dir: float_3) = {
    new SW_Ray(_ori, _dir)
  }
}

/**
 *  Default "constructor" places the box at +Inf, so that any ray will not
 *  intersect with it. This is better than placing the default box at the point
 *  of origin, which usually get calculated to be intersecting with a ray that
 *  passes through the origin. It is common in human-written test cases to have
 *  a origin-intersecting ray, and it is common too that some boxes are not
 *  specified because the human just wanted to test a few boxes instead of all
 *  four.  
 */
case class SW_Box(
    val x_min: Float = Float.PositiveInfinity,
    val x_max: Float = Float.PositiveInfinity,
    val y_min: Float = Float.PositiveInfinity,
    val y_max: Float = Float.PositiveInfinity,
    val z_min: Float = Float.PositiveInfinity,
    val z_max: Float = Float.PositiveInfinity
)

case class SW_CombinedData(
  val ray: SW_Ray ,
  val boxes: Seq[SW_Box],
  val isTriangleOp: Boolean
)

/**
  * A bunch of implicit conversion methods between SW and HW data types
  */
object RaytracerTestHelper {
  // implicit class TestableHWRay(dut_ray: baseline_datapath.Ray) {
  //   def poke(sw_ray: SW_Ray): Unit = {
  //     assert(!dut_ray.isRecordedFloat())
  //     dut_ray.origin.x.poke(floatToBits(sw_ray.origin.x))
  //     dut_ray.origin.y.poke(floatToBits(sw_ray.origin.y))
  //     dut_ray.origin.z.poke(floatToBits(sw_ray.origin.z))

  //     dut_ray.dir.x.poke(floatToBits(sw_ray.dir.x))
  //     dut_ray.dir.y.poke(floatToBits(sw_ray.dir.y))
  //     dut_ray.dir.z.poke(floatToBits(sw_ray.dir.z))

  //     dut_ray.inv.x.poke(floatToBits(sw_ray.inv.x))
  //     dut_ray.inv.y.poke(floatToBits(sw_ray.inv.y))
  //     dut_ray.inv.z.poke(floatToBits(sw_ray.inv.z))

  //     dut_ray.extent.poke(floatToBits(sw_ray.extent))
  //   }
  // }

  // implicit class TestableHWAABB(dut_box: baseline_datapath.AABB) {
  //   def poke(sw_box: SW_Box): Unit = {
  //     assert(!dut_box.isRecordedFloat())
  //     dut_box.x_min.poke(floatToBits(sw_box.x_min))
  //     dut_box.y_min.poke(floatToBits(sw_box.y_min))
  //     dut_box.z_min.poke(floatToBits(sw_box.z_min))

  //     dut_box.x_max.poke(floatToBits(sw_box.x_max))
  //     dut_box.y_max.poke(floatToBits(sw_box.y_max))
  //     dut_box.z_max.poke(floatToBits(sw_box.z_max))
  //   }
  // }

  // // Whereas method poke(...) is called, expecting a RayBoxPair as argument,
  // // instead sees a SW_Ray and a SW_Box as arguments, this implicit conversion
  // // will be made.
  // implicit class TestableRayBoxPair(dut_ray_box_pair: baseline_datapath.RayBoxPair) {
  //   def poke(sw_ray: SW_Ray, sw_box : SW_Box) : Unit = {
  //     dut_ray_box_pair.ray.poke(sw_ray)
  //     dut_ray_box_pair.aabb.poke(sw_box)
  //   }
  // }

  // implicit class TestableRayBoxTriangleBundle(dut_combined_port: baseline_datapath.CombinedRayBoxTriangleBundle) {
  //   def poke(s: SW_CombinedData) : Unit = {
  //     assert(s.boxes.length == 4)

  //     dut_combined_port.isTriangleOp.poke(s.isTriangleOp)
  //     dut_combined_port.ray.poke(s.ray)
  //     for(i <- 0 until 4){
  //       dut_combined_port.aabb(i).poke(s.boxes(i))
  //     }
  //   }
  // }

  // Whereas a RayBoxPair is expected, but see instead a tuple[SW_Ray, SW_Box],
  // this implicit conversion will be made.
  implicit def fromSWRayAndSWBoxToRayBoxPair(rb: (SW_Ray, SW_Box)): RayBoxPair = {
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

  implicit def fromSWRaySWBoxSeqToRayBoxPairSeq(rbseq: Seq[(SW_Ray, SW_Box)]) : Seq[RayBoxPair] = {
    rbseq.map(fromSWRayAndSWBoxToRayBoxPair(_))
  }

  implicit def fromSWRayAndSWBoxesToCombinedRayBoxTriangleBundle(rb: SW_CombinedData): CombinedRayBoxTriangleBundle = {
    import chisel3.experimental.BundleLiterals._
    val sw_ray= rb.ray
    val sw_box = rb.boxes
    val op = rb.isTriangleOp 
    assert(sw_box.length == 4)

    lazy val dummy_crbtb = new CombinedRayBoxTriangleBundle(false)
    lazy val dummy_aabb = new AABB(false)

    dummy_crbtb.Lit(
      _.isTriangleOp -> op.B,
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
      _.aabb(0) -> 
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(0).x_min),
          _.x_max -> floatToBits(sw_box(0).x_max),
          _.y_min -> floatToBits(sw_box(0).y_min),
          _.y_max -> floatToBits(sw_box(0).y_max),
          _.z_min -> floatToBits(sw_box(0).z_min),
          _.z_max -> floatToBits(sw_box(0).z_max),
        ),
      _.aabb(1) -> 
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(1).x_min),
          _.x_max -> floatToBits(sw_box(1).x_max),
          _.y_min -> floatToBits(sw_box(1).y_min),
          _.y_max -> floatToBits(sw_box(1).y_max),
          _.z_min -> floatToBits(sw_box(1).z_min),
          _.z_max -> floatToBits(sw_box(1).z_max),
        ),
      _.aabb(2) -> 
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(2).x_min),
          _.x_max -> floatToBits(sw_box(2).x_max),
          _.y_min -> floatToBits(sw_box(2).y_min),
          _.y_max -> floatToBits(sw_box(2).y_max),
          _.z_min -> floatToBits(sw_box(2).z_min),
          _.z_max -> floatToBits(sw_box(2).z_max),
        ),
      _.aabb(3) -> 
        dummy_aabb.Lit(
          _.x_min -> floatToBits(sw_box(3).x_min),
          _.x_max -> floatToBits(sw_box(3).x_max),
          _.y_min -> floatToBits(sw_box(3).y_min),
          _.y_max -> floatToBits(sw_box(3).y_max),
          _.z_min -> floatToBits(sw_box(3).z_min),
          _.z_max -> floatToBits(sw_box(3).z_max),
        ),
    )
  }

  implicit def fromSWRayAndSWBoxesSeqToCombinedRayBoxTriangleBundleSeq(rbseq: Seq[SW_CombinedData]): Seq[CombinedRayBoxTriangleBundle] = {
    rbseq.map(fromSWRayAndSWBoxesToCombinedRayBoxTriangleBundle(_))
  }
}

object RaytracerGold {

  /** Performs the CPU calculation of ray-box intersection testing
    *
    * @param ray
    * @param box
    * @return
    *   None if no intersection, else Some[Float] containing the tmin value
    */
  def testIntersection(ray: SW_Ray, box: SW_Box): Option[Float] = {
    /* Step 1: translate box relative to ray origin */
    val translated_box = new SW_Box(
      x_min = box.x_min - ray.origin.x,
      y_min = box.y_min - ray.origin.y,
      z_min = box.z_min - ray.origin.z,
      x_max = box.x_max - ray.origin.x,
      y_max = box.y_max - ray.origin.y,
      z_max = box.z_max - ray.origin.z
    )

    /* Step 2: Time intersection interval calculations for each axis plane */
    val tp_min_x: Float = translated_box.x_min * ray.inv.x
    val tp_min_y: Float = translated_box.y_min * ray.inv.y
    val tp_min_z: Float = translated_box.z_min * ray.inv.z
    val tp_max_x: Float = translated_box.x_max * ray.inv.x
    val tp_max_y: Float = translated_box.y_max * ray.inv.y
    val tp_max_z: Float = translated_box.z_max * ray.inv.z

    /* Step 3: Flip the intervals if the raydir is negative along that axis */
    val t_min_x: Float = if (ray.dir.x >= 0) { tp_min_x }
    else { tp_max_x }
    val t_max_x: Float = if (ray.dir.x >= 0) { tp_max_x }
    else { tp_min_x }
    val t_min_y: Float = if (ray.dir.y >= 0) { tp_min_y }
    else { tp_max_y }
    val t_max_y: Float = if (ray.dir.y >= 0) { tp_max_y }
    else { tp_min_y }
    val t_min_z: Float = if (ray.dir.z >= 0) { tp_min_z }
    else { tp_max_z }
    val t_max_z: Float = if (ray.dir.z >= 0) { tp_max_z }
    else { tp_min_z }

    var tmin: Float = max(t_min_x, t_min_y)
    tmin = max(tmin, t_min_z)
    tmin = max(tmin, 0.0f)

    var tmax: Float = min(t_max_x, t_max_y)
    tmax = min(tmax, t_max_z)
    tmax = min(tmax, ray.extent)

    val retval = if (tmin < tmax) {
      Some(tmin)
    } else None

    retval
  }

  case class SW_RayBox_Result(
    val t_min: Seq[Float],
    val is_intersect: Seq[Boolean],
    val box_index: Seq[Int]
  )

  // Non-intersections are marked as PositiveInfinity
  def testIntersection(ray: SW_Ray, box_seq: Seq[SW_Box]): SW_RayBox_Result = {
    val four_results: Seq[Option[Float]] = box_seq.map(testIntersection(ray, _))

    val unsorted_result = SW_RayBox_Result(
      t_min = four_results.map(_.getOrElse(Float.PositiveInfinity)),
      is_intersect = four_results.map(_.nonEmpty),
      box_index = (0 until 4).toList
    )

    val sorted_result = unsorted_result.t_min.zip(unsorted_result.is_intersect).zip(unsorted_result.box_index).sortWith{
      case(((t1, b1), idx1), ((t2, b2), idx2)) => t1 < t2
    }

    SW_RayBox_Result(
      t_min = sorted_result.map(_._1._1),
      is_intersect = sorted_result.map(_._1._2),
      box_index = sorted_result.map(_._2)
    )
  }

  /**
    * Generate a randomized AABB given the range
    * -range <= x_min, x_max, y_min, y_max, z_min, z_max <= range
    */
  def genRandomBox(range: Float): SW_Box = {
    import scala.util.Random
    lazy val r = new Random()
    assert(range > 0.0f)
    val rands = (0 until 6).map{_=>r.nextFloat()*range*2 - range}
    val b = new SW_Box(
      min(rands(0), rands(1)), max(rands(0), rands(1)),
      min(rands(2), rands(3)), max(rands(2), rands(3)),
      min(rands(4), rands(5)), max(rands(4), rands(5)),
      )
    assert(b.x_min <= b.x_max)
    assert(b.y_min <= b.y_max)
    assert(b.z_min <= b.z_max)
    b
  }

  /**
    * Generate a randomized ray originating from a point within range
    * -range <= origin.x, origin.y, origin.z <= range
    *
    * The x/y/z direction of ray is between -range and range  
    * @param range
    * @return
    */
  def genRandomRay(range: Float): SW_Ray = {
    import scala.util.Random
    lazy val r = new Random() 
    assert(range > 0.0f)
    val rands = (0 until 6).map{_=>r.nextFloat()*2*range - range}
    val ray = new SW_Ray(
      float_3(rands(0), rands(1), rands(2)),
      float_3(rands(3), rands(4), rands(5))
    )

    ray
  }
}
