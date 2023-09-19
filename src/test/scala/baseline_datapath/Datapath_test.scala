package baseline_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scala.math._
import baseline_datapath.raytracer_gold._
import chiseltest.experimental.expose

import RaytracerTestHelper._

import java.nio.ByteBuffer
import chiseltest.internal.CachingAnnotation
import chiseltest.simulator.CachingDebugAnnotation
import firrtl2.options.TargetDirAnnotation
import chisel3.stage.{PrintFullStackTraceAnnotation,ThrowOnFirstErrorAnnotation}

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

class Datapath_wrapper extends Datapath {
  import hardfloat._
  val exposed_tmin = expose(fNFromRecFN(8, 24, tmin))
  val exposed_tmax = expose(fNFromRecFN(8, 24, tmax))
  val exposed_aabb_3 = expose(AABBConvertRecFNtoFN(aabb_3))
  val exposed_ray_2 = expose(RayConvertRecFNtoFN(ray_2))
  val exposed_adder_exception_0 = expose(adder_exceptions(0))
  val exposed_tmin_3d = expose(Float3ConvertRecFNtoFN(tmin_3d))
  val exposed_tmax_3d = expose(Float3ConvertRecFNtoFN(tmax_3d))
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester {

  // we are dealing with hardware and software Ray types
  type HW_Ray = baseline_datapath.Ray
  type HW_Box = baseline_datapath.AABB

  val r = new Random()
  val N_RANDOM_TEST = 50000

  // Define a function for ray-box intersection testing
  def testRayBoxIntersection(
      description: String,
      box_seq: Seq[SW_Box],
      ray_seq: Seq[SW_Ray]
  ): Unit = {
    description in {
      test(new Datapath_wrapper).withAnnotations(
        Seq(
          VerilatorBackendAnnotation,
          CachingAnnotation,
          CachingDebugAnnotation,
          TargetDirAnnotation("cached_verilator_backend/Datapath"),
        )
      ).withChiselAnnotations(
        Seq(
          ThrowOnFirstErrorAnnotation
        )
      ) { dut =>
        for(box <- box_seq; ray <- ray_seq) {
          // Test for intersection
          val result = RaytracerGold.testIntersection(ray, box)
          val expectedIntersect = result.nonEmpty
          dut.in.bits.poke(ray, box)
          dut.in.valid.poke(true.B)
          dut.clock.step(6)

          // Expect the intersection result to match the expected value
          dut.out.bits.isIntersect.expect(expectedIntersect.B)

          // Print the expected and actual tmin values
          // println(s"$description - expected tmin is ${result.getOrElse(
          //     -1.0f
          //   )}, actual tmin_out is ${bitsToFloat(dut.io.tmin_out.peek())}")
        }
      }
    }
  }

  // Define test cases
  testRayBoxIntersection(
    "Small Ray inside box",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(0.5f, -0.5f, 0.5f), float_3(0.0001f, -0.0001f, -0.001f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing away",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on edge of box pointing away",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing away",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing along edge",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing towards box",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray hits node 1 then node 2",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
      new SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f, 0.5f) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray hits node 4 then 1 then 2 then misses 3",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
      new SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      new SW_Box(100.0f, 100.25f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      new SW_Box(-5.0f, -4.0f, -0.5f, 0.5f, -0.5f, 0.5f) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray outside box pointing along edge",
    new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: Nil,
    new SW_Ray(float_3(1.0f, 2.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    s"${N_RANDOM_TEST} randomized rays and boxes within range -10000.0, 10000.0",
    List.fill(math.sqrt(N_RANDOM_TEST).toInt){RaytracerGold.genRandomBox(10000)},
    List.fill(math.sqrt(N_RANDOM_TEST).toInt){RaytracerGold.genRandomRay(10000)}
  )
}
