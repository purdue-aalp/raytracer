package baseline_datapath.raytracer_gold

import scala.math._
import scala.util.Random
object SW_Opcode extends Enumeration {
  type SW_Opcode = Value
  val SW_OpTriangle = Value(0)
  val SW_OpQuadbox = Value(1)
  val SW_OpEuclidean = Value(2)
  val SW_OpAngular = Value(3)
}

import SW_Opcode._

case class float_3(
    val x: Float,
    val y: Float,
    val z: Float
) {
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

    if (y > maxVal) {
      maxVal = y
      maxInd = 1
    }
    if (z > maxVal) {
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
    var _kx = if (_kz + 1 == 3) { 0 }
    else { _kz + 1 }
    var _ky = if (_kx + 1 == 3) { 0 }
    else { _kx + 1 }
    if (dir.at(_kz) < 0.0f) {
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

  override def toString(): String = {
    f"${this.getClass().getName()}(origin(${origin.toString()}), dir(${dir
        .toString()}), kx($kx), ky($ky), kz($kz), shear(${shear.toString()}))"
  }
}

object SW_Ray {
  val SCENE_BOUNDS = 1000
  val RAY_EXTENT = 4 * SCENE_BOUNDS
  def apply(_ori: float_3, _dir: float_3) = {
    new SW_Ray(_ori, _dir)
  }
}

/** Default "constructor" places the box at +Inf, so that any ray will not
  * intersect with it. This is better than placing the default box at the point
  * of origin, which usually get calculated to be intersecting with a ray that
  * passes through the origin. It is common in human-written test cases to have
  * a origin-intersecting ray, and it is common too that some boxes are not
  * specified because the human just wanted to test a few boxes instead of all
  * four.
  */
case class SW_Box(
    val x_min: Float = Float.PositiveInfinity,
    val x_max: Float = Float.PositiveInfinity,
    val y_min: Float = Float.PositiveInfinity,
    val y_max: Float = Float.PositiveInfinity,
    val z_min: Float = Float.PositiveInfinity,
    val z_max: Float = Float.PositiveInfinity
)

case class SW_Triangle(
    val A: float_3 = float_3(0.0f, 0.0f, 0.0f),
    val B: float_3 = float_3(0.0f, 0.0f, 0.0f),
    val C: float_3 = float_3(0.0f, 0.0f, 0.0f)
) {
  def centroid: float_3 = float_3(
    (A.x + B.x + C.x) / 3.0f,
    (A.y + B.y + C.y) / 3.0f,
    (A.z + B.z + C.z) / 3.0f
  )
}

class SW_Vector(val elements: Seq[Float]) {
  val dim = elements.size

  def get_elements(): Seq[Float] = elements

  override def toString(): String = {
    elements.foldLeft("(") { case (str, ele) => str + s"${ele}, " } + ")"
  }

  def calc_diff(other: SW_Vector): Float = {
    assert(other.dim == dim)
    elements
      .zip(other.get_elements())
      .map { case (a, b) =>
        (a - b) * (a - b)
      }
      .reduce(_ + _)
  }

  def calc_dot_product(other: SW_Vector): Float = {
    assert(other.dim == dim)
    elements
      .zip(other.get_elements())
      .map { case (a, b) =>
        a * b
      }
      .reduce(_ + _)
  }

  def get_norm(): Float = {
    calc_dot_product(this)
  }
}

object SW_Vector {
  val DEFAULT_SIZE = 16
  def apply(elements: Seq[Float] = Seq.fill[Float](DEFAULT_SIZE)(0.0f)) = {
    new SW_Vector(elements = elements)
  }
}

case class SW_CombinedData(
    val ray: SW_Ray,
    val boxes: Seq[SW_Box],
    val triangle: SW_Triangle,
    val opcode: SW_Opcode
)

case class SW_EnhancedCombinedData(
    val ray: SW_Ray,
    val boxes: Seq[SW_Box],
    val triangle: SW_Triangle,
    val opcode: SW_Opcode,
    val element_count: Option[Int],
    val vector_a: SW_Vector,
    val vector_b: SW_Vector,
    val reset_accum: Boolean,
    val vector_mask: Int
)

case class SW_RayBox_Result(
    val t_min: Seq[Float] = Nil,
    val is_intersect: Seq[Boolean] = Nil,
    val box_index: Seq[Int] = Nil
)

case class SW_RayTriangle_Result(
    val t_num: Float = 0.0f,
    val t_denom: Float = 0.0f,
    val is_hit: Boolean = false
)

case class SW_Unified_Result(
    val isTriangle: Boolean,
    val triangle_result: SW_RayTriangle_Result,
    val box_result: SW_RayBox_Result
)

/// The two SW_Vectors may be of any length (not multiples of 16). This method
/// breaks down and transforms the SW_Vector into a sequence of
/// SW_EnhancedCombinedData objects, each of which contains only a pair of
/// 16-length SW_Vectors, and a bit mask indicating which elements of the
/// SW_Vectors are valid.
/// E.g. If the input SW_Vector has dim==17, the first object in the return
/// sequence has a fully-on bit mask (0xffff), and the second object has a mask
/// of 0xc000.
object get_euclidean_job_seq_from_vec_pair {
  // Given n in range [1, 16], returns a 16-bit mask encoded in an Int.
  // Bits (15) ~ (15-n-1) are marked as one, all other bits in the mask are
  // zero.
  // E.g. n=3 => mask = 0x0007
  def generate_mask(n: Int): Int = {
    def get_single_bit_mask(n: Int): Int = {
      // n      = 1, 2, 3, ... 16
      // retval = 0001, 0002, 0004, ... 8000
      assert(0 < n, "n must be positive")
      assert(n <= 16, "n must be less than 17")
      1 << (n - 1)
    }
    // masks(0) is meaningless, masks(1~16) relate to n=1~16
    lazy val masks: Seq[Int] = Seq.tabulate(17) { idx =>
      (1 to idx).foldLeft(0) { case (l, r) => l + get_single_bit_mask(r) }
    }
    masks(n)
  }

  def apply(
      vec_a: SW_Vector,
      vec_b: SW_Vector
  ): Seq[SW_EnhancedCombinedData] = {
    assert(vec_b.dim == vec_a.dim)
    import scala.math._
    val beats: Int = (vec_a.dim / 16.0f).ceil.toInt

    // a recursive way to convert arbitrarily long vectors into a seq of SW_EnhancedCombinedData
    def job_seq(
        vec_pair: (Seq[Float], Seq[Float])
    ): Seq[SW_EnhancedCombinedData] = vec_pair match {
      case (Nil, Nil) => Nil
      case _ =>
        val (vec_a_first_beat, vec_a_remaining) = vec_pair._1.splitAt(16)
        val (vec_b_first_beat, vec_b_remaining) = vec_pair._2.splitAt(16)
        val last_beat = if (vec_pair._1.length <= 16) true else false
        val vec_length = vec_a_first_beat.length

        val one_job = SW_EnhancedCombinedData(
          SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(1.0f, 1.0f, 1.0f)),
          Seq.fill(4)(SW_Box()),
          SW_Triangle(),
          SW_OpEuclidean,
          Some(16),
          SW_Vector(vec_a_first_beat.padTo(16, 0.0f)),
          SW_Vector(vec_b_first_beat.padTo(16, 0.0f)),
          last_beat,
          generate_mask(vec_length)
        )
        one_job +: job_seq((vec_a_remaining, vec_b_remaining))
    }

    job_seq((vec_a.get_elements(), vec_b.get_elements()))

  }
}

object get_angular_job_seq_from_vec_pair {
  def apply(
      vec_a: SW_Vector,
      vec_b: SW_Vector
  ): Seq[SW_EnhancedCombinedData] = {
    assert(vec_b.dim == vec_a.dim)
    import scala.math._
    val beats: Int = (vec_a.dim / 8.0f).ceil.toInt

    // a recursive way to convert arbitrarily long vectors into a seq of SW_EnhancedCombinedData
    def job_seq(
        vec_pair: (Seq[Float], Seq[Float])
    ): Seq[SW_EnhancedCombinedData] = vec_pair match {
      case (Nil, Nil) => Nil
      case _ =>
        val (vec_a_first_beat, vec_a_remaining) = vec_pair._1.splitAt(8)
        val (vec_b_first_beat, vec_b_remaining) = vec_pair._2.splitAt(8)
        val last_beat = if (vec_pair._1.length <= 8) true else false
        val vec_length = vec_a_first_beat.length

        val one_job = SW_EnhancedCombinedData(
          SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(1.0f, 1.0f, 1.0f)),
          Seq.fill(4)(SW_Box()),
          SW_Triangle(),
          SW_OpAngular,
          Some(16),
          SW_Vector(vec_a_first_beat.padTo(16, 0.0f)),
          SW_Vector(vec_b_first_beat.padTo(16, 0.0f)),
          last_beat,
          get_euclidean_job_seq_from_vec_pair.generate_mask(vec_length)
        )
        one_job +: job_seq((vec_a_remaining, vec_b_remaining))
    }

    job_seq((vec_a.get_elements(), vec_b.get_elements()))

  }
}

object RandomSWData {

  /** Generate a randomized AABB given the range -range <= x_min, x_max, y_min,
    * y_max, z_min, z_max <= range
    */
  def genRandomBox(range: Float, r: Random = new Random()): SW_Box = {
    assert(range > 0.0f)
    val rands = (0 until 6).map { _ => r.nextFloat() * range * 2 - range }
    val b = new SW_Box(
      min(rands(0), rands(1)),
      max(rands(0), rands(1)),
      min(rands(2), rands(3)),
      max(rands(2), rands(3)),
      min(rands(4), rands(5)),
      max(rands(4), rands(5))
    )
    assert(b.x_min <= b.x_max)
    assert(b.y_min <= b.y_max)
    assert(b.z_min <= b.z_max)
    b
  }

  /** Generate a randomized ray originating from a point within range -range <=
    * origin.x, origin.y, origin.z <= range
    *
    * The x/y/z direction of ray is between -range and range
    * @param range
    * @return
    */
  def genRandomRay(range: Float, r: Random = new Random()): SW_Ray = {
    assert(range > 0.0f)
    val rands = (0 until 6).map { _ => r.nextFloat() * 2 * range - range }
    val ray = new SW_Ray(
      float_3(rands(0), rands(1), rands(2)),
      float_3(rands(3), rands(4), rands(5))
    )

    ray
  }

  def genRandomRayGivenPoint(
      point: float_3,
      lower_bound: Float,
      upper_bound: Float,
      r: Random = new Random()
  ): SW_Ray = {
    assert(upper_bound > lower_bound)

    var origin = point
    while (origin == point) {
      val rands = (0 until 3).map { _ =>
        r.nextFloat() * (upper_bound - lower_bound) + lower_bound
      }
      origin = float_3(rands(0), rands(1), rands(2))
    }

    val dir =
      float_3(point.x - origin.x, point.y - origin.y, point.z - origin.z)

    val ray = new SW_Ray(origin, dir)
    ray
  }

  def genRandomTriangle(
      lower_bound: Float,
      upper_bound: Float,
      r: Random = new Random()
  ): SW_Triangle = {
    assert(upper_bound > lower_bound)
    val rands = (0 until 9).map { _ =>
      r.nextFloat() * (upper_bound - lower_bound) + lower_bound
    }

    SW_Triangle(
      float_3(rands(0), rands(1), rands(2)),
      float_3(rands(3), rands(4), rands(5)),
      float_3(rands(6), rands(7), rands(8))
    )
  }

  // the SW_Vector returned from this method is arbitrarily long.
  def genRandomVector(
      lower_bound: Float,
      upper_bound: Float,
      length: Int,
      r: Random = new Random()
  ): SW_Vector = {

    SW_Vector(
      Seq.fill(length)(
        r.nextFloat() * (upper_bound - lower_bound) + lower_bound
      )
    )
  }

  def genRandomVectorPair(
      lower_bound: Float,
      upper_bound: Float,
      largest_length: Int,
      r: Random = new Random()
  ): (SW_Vector, SW_Vector) = {
    val _length = r.nextInt(largest_length) + 1
    (
      genRandomVector(lower_bound, upper_bound, _length),
      genRandomVector(lower_bound, upper_bound, _length)
    )
  }
}
