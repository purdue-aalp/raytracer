package baseline_datapath.raytracer_gold

import scala.math._

class float_3 (
  val x: Float,
  val y: Float,
  val z: Float
)

// companion object for class float_3
object float_3{
  def apply(_x: Float, _y: Float, _z: Float) = {
    new float_3(_x, _y, _z)
  }
}

class Ray (
  val origin: float_3,
  val dir: float_3
){
  val extent: Float = Ray.RAY_EXTENT.toFloat
  val inv: float_3 = {
    val _x = 1.0f / dir.x
    val _y = 1.0f / dir.y 
    val _z = 1.0f / dir.z
    float_3(_x, _y, _z)
  }
}

object Ray {
  val SCENE_BOUNDS = 100000
  val RAY_EXTENT = 4 * SCENE_BOUNDS
  def apply(_ori: float_3, _dir: float_3)={
    new Ray(_ori, _dir)
  }
}

class Box(
  val x_min: Float,
  val x_max: Float,
  val y_min: Float,
  val y_max: Float,
  val z_min: Float,
  val z_max: Float
)

object Box{
  def apply(
    _xmin: Float, _xmax: Float, 
    _ymin: Float, _ymax: Float,
    _zmin: Float, _zmax: Float
  ) = {
    new Box(_xmin, _xmax, _ymin, _ymax, _zmin, _zmax)
  }
}

object RaytracerGold {
  /**
    * Performs the CPU calculation of ray-box intersection testing
    *
    * @param ray
    * @param box
    * @return None if no intersection, else Some[Float] containing the tmin
    * value 
    */
  def testIntersection(ray: Ray, box: Box): Option[Float] = {
    /* Step 1: translate box relative to ray origin */
    val translated_box = new Box(
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
    val t_min_x: Float = if(ray.dir.x >= 0){tp_min_x}else{tp_max_x}
    val t_max_x: Float = if(ray.dir.x >= 0){tp_max_x}else{tp_min_x}
    val t_min_y: Float = if(ray.dir.y >= 0){tp_min_y}else{tp_max_y}
    val t_max_y: Float = if(ray.dir.y >= 0){tp_max_y}else{tp_min_y}
    val t_min_z: Float = if(ray.dir.z >= 0){tp_min_z}else{tp_max_z}
    val t_max_z: Float = if(ray.dir.z >= 0){tp_max_z}else{tp_min_z}

    var tmin: Float = max(t_min_x, t_min_y)
    tmin = max(tmin, t_min_z)
    tmin = max(tmin, 0.0f)

    var tmax: Float = min(t_max_x, t_max_y)
    tmax = min(tmax, t_max_z)
    tmax = min(tmax, ray.extent)


    val retval = if(tmin <= tmax){
      Some(tmin)
    } else None

    retval
  }
}
