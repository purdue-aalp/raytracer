// This file is part of raytracer_chisel.
// Licensed under the BSD 3-clause License.
// See the LICENSE.txt file for details.

package raytracer_datapath.raytracer_gold

import chisel3._
import scala.math._

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

  // Non-intersections are marked as PositiveInfinity
  def testIntersection(ray: SW_Ray, box_seq: Seq[SW_Box]): SW_RayBox_Result = {
    val four_results: Seq[Option[Float]] = box_seq.map(testIntersection(ray, _))

    val unsorted_result = SW_RayBox_Result(
      t_min = four_results.map(_.getOrElse(Float.PositiveInfinity)),
      is_intersect = four_results.map(_.nonEmpty),
      box_index = (0 until 4).toList
    )

    val sorted_result = unsorted_result.t_min
      .zip(unsorted_result.is_intersect)
      .zip(unsorted_result.box_index)
      .sortWith { case (((t1, b1), idx1), ((t2, b2), idx2)) =>
        t1 < t2
      }

    SW_RayBox_Result(
      t_min = sorted_result.map(_._1._1),
      is_intersect = sorted_result.map(_._1._2),
      box_index = sorted_result.map(_._2)
    )
  }

  def testTriangleIntersection(
      ray: SW_Ray,
      triangle: SW_Triangle
  ): SW_RayTriangle_Result = {
    lazy val default_result = SW_RayTriangle_Result()
    var result = default_result

    val (kx, ky, kz) = (ray.kx, ray.ky, ray.kz)
    val A = float_3(
      triangle.A.x - ray.origin.x,
      triangle.A.y - ray.origin.y,
      triangle.A.z - ray.origin.z
    )
    val B = float_3(
      triangle.B.x - ray.origin.x,
      triangle.B.y - ray.origin.y,
      triangle.B.z - ray.origin.z
    )
    val C = float_3(
      triangle.C.x - ray.origin.x,
      triangle.C.y - ray.origin.y,
      triangle.C.z - ray.origin.z
    )

    val Ax = A.at(kx) - ray.shear.x * A.at(kz)
    val Ay = A.at(ky) - ray.shear.y * A.at(kz)
    val Bx = B.at(kx) - ray.shear.x * B.at(kz)
    val By = B.at(ky) - ray.shear.y * B.at(kz)
    val Cx = C.at(kx) - ray.shear.x * C.at(kz)
    val Cy = C.at(ky) - ray.shear.y * C.at(kz)

    val U = Cx * By - Cy * Bx
    val V = Ax * Cy - Ay * Cx
    val W = Bx * Ay - By * Ax

    if (U < 0.0f || V < 0.0f || W < 0.0f) {
      // pass
    } else {
      result = result.copy(t_denom = U + V + W)
      if (result.t_denom == 0.0f) {
        // pass
      } else {
        val Az = ray.shear.z * A.at(kz)
        val Bz = ray.shear.z * B.at(kz)
        val Cz = ray.shear.z * C.at(kz)

        result = result.copy(t_num = U * Az + V * Bz + W * Cz)
        if (result.t_num < 0.0f) {
          // pass
        } else {
          result = result.copy(is_hit = true)
        }
      }
    }

    result
  }

}
