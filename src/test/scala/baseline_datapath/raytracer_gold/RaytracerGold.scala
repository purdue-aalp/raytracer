package baseline_datapath.raytracer_gold

import scala.math._
import baseline_datapath.floatToBits
import chiseltest.testableUInt
import scala.language.implicitConversions

case class float_3(
    val x: Float,
    val y: Float,
    val z: Float
)

class SW_Ray(
    val origin: float_3,
    val dir: float_3
) {
  val extent: Float = SW_Ray.RAY_EXTENT.toFloat
  val inv: float_3 = {
    val _x = 1.0f / dir.x
    val _y = 1.0f / dir.y
    val _z = 1.0f / dir.z
    val _inv = float_3(_x, _y, _z)
    _inv
  }
}

object SW_Ray {
  val SCENE_BOUNDS = 100000
  val RAY_EXTENT = 4 * SCENE_BOUNDS
  def apply(_ori: float_3, _dir: float_3) = {
    new SW_Ray(_ori, _dir)
  }
}

case class SW_Box(
    val x_min: Float,
    val x_max: Float,
    val y_min: Float,
    val y_max: Float,
    val z_min: Float,
    val z_max: Float
)

object RaytracerTestHelper {
  implicit class TestableHWRay(dut_ray: baseline_datapath.Ray) {
    def poke(sw_ray: SW_Ray): Unit = {
      assert(!dut_ray.isRecordedFloat())
      dut_ray.origin.x.poke(floatToBits(sw_ray.origin.x))
      dut_ray.origin.y.poke(floatToBits(sw_ray.origin.y))
      dut_ray.origin.z.poke(floatToBits(sw_ray.origin.z))

      dut_ray.dir.x.poke(floatToBits(sw_ray.dir.x))
      dut_ray.dir.y.poke(floatToBits(sw_ray.dir.y))
      dut_ray.dir.z.poke(floatToBits(sw_ray.dir.z))

      dut_ray.inv.x.poke(floatToBits(sw_ray.inv.x))
      dut_ray.inv.y.poke(floatToBits(sw_ray.inv.y))
      dut_ray.inv.z.poke(floatToBits(sw_ray.inv.z))

      dut_ray.extent.poke(floatToBits(sw_ray.extent))
    }
  }

  implicit class TestableHWAABB(dut_box: baseline_datapath.AABB) {
    def poke(sw_box: SW_Box): Unit = {
      assert(!dut_box.isRecordedFloat())
      dut_box.x_min.poke(floatToBits(sw_box.x_min))
      dut_box.y_min.poke(floatToBits(sw_box.y_min))
      dut_box.z_min.poke(floatToBits(sw_box.z_min))

      dut_box.x_max.poke(floatToBits(sw_box.x_max))
      dut_box.y_max.poke(floatToBits(sw_box.y_max))
      dut_box.z_max.poke(floatToBits(sw_box.z_max))
    }
  }

  implicit class TestableRayBoxPair(dut_ray_box_pair: baseline_datapath.RayBoxPair) {
    def poke(sw_ray: SW_Ray, sw_box : SW_Box) : Unit = {
      dut_ray_box_pair.ray.poke(sw_ray)
      dut_ray_box_pair.aabb.poke(sw_box)
    }
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
