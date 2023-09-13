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

object floatToBits{
  /**
    * Given a Scala float, returns an Int directly casted from the bits of the
    * float.
    * 
    * Similar to C-code: int x; return y = *((float*)(&x));
    *
    * @param x A Float value
    * @return The integer cast of x.
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

object bitsToFloat{
  def apply(x: UInt): Float = {
    assert(x.getWidth == 32)
    val bb: ByteBuffer = ByteBuffer.allocate(4)
    bb.putInt(x.litValue.intValue)
    val y = bb.rewind().getFloat()
    y
  }
}


class Datapath_wrapper extends Datapath{
  import hardfloat._
  val exposed_tmin = expose(fNFromRecFN(8,24, tmin))
  val exposed_tmax = expose(fNFromRecFN(8,24, tmax))
  val exposed_aabb_3 = expose(AABBConvertRecFNtoFN(aabb_3))
  val exposed_ray_2 = expose(RayConvertRecFNtoFN(ray_2))
  val exposed_adder_exception_0 = expose(adder_exceptions(0))
  val exposed_tmin_3d = expose(Float3ConvertRecFNtoFN(tmin_3d))
  val exposed_tmax_3d = expose(Float3ConvertRecFNtoFN(tmax_3d))
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester{

  // we are dealing with hardware and software Ray types
  type HW_Ray = baseline_datapath.Ray
  type HW_Box = baseline_datapath.AABB

  val r = new Random()

  // Define a function for ray-box intersection testing
  def testRayBoxIntersection(description: String, box: SW_Box, ray: SW_Ray): Unit = {
    description in {
      // Test for intersection
      val result = RaytracerGold.testIntersection(ray, box)
      val expectedIntersect = result.nonEmpty

      // Perform testing using your hardware description language (HDL) framework
      test(new Datapath_wrapper) { dut =>
        dut.io.ray.poke(ray)
        dut.io.aabb.poke(box)
        dut.clock.step(6)

        // Expect the intersection result to match the expected value
        dut.io.isIntersect.expect(expectedIntersect.B)

        // Print the expected and actual tmin values
        println(s"$description - expected tmin is ${result.getOrElse(-1.0f)}, actual tmin_out is ${bitsToFloat(dut.io.tmin_out.peek())}")
      }
    }
  }

  // Define test cases
  val box = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)

  testRayBoxIntersection("Small Ray inside box", box, new SW_Ray(float_3(0.5f, -0.5f, 0.5f), float_3(0.0001f, -0.0001f, -0.001f)))
  testRayBoxIntersection("Ray outside box pointing away", box, new SW_Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)))
  testRayBoxIntersection("Ray on edge of box pointing away", box, new SW_Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)))
  testRayBoxIntersection("Ray on corner of box pointing away", box, new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f)))
  testRayBoxIntersection("Ray on corner of box pointing along edge", box, new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)))
  testRayBoxIntersection("Ray outside box pointing towards box", box, new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)))


// "Small Ray inside box" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(0.5f, -0.5f, 0.5f), float_3(0.0001f, -0.0001f, -0.001f))
  
//   val result = RaytracerGold.testIntersection(r, b)
//   val expected_intersect = if(result.nonEmpty){true.B} else {false.B}

//   test(new Datapath_wrapper){dut =>
//     dut.io.ray.poke(r)
//     dut.io.aabb.poke(b)
//     dut.clock.step(6)

//     dut.io.isIntersect.expect(expected_intersect)
//     println(s"expected tmin is ${result.getOrElse(-1.0f)}, actual tmin_out is ${bitsToFloat(dut.io.tmin_out.peek())}")
//   }
// }

// "Ray outside box pointing away" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
//   val result = RaytracerGold.testIntersection(r, b)
//   print(result)
// }

// "Ray on edge of box pointing away" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
//   val result = RaytracerGold.testIntersection(r, b)
//   print(result)
// }

// "Ray on corner of box pointing away" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f))
//   val result = RaytracerGold.testIntersection(r, b)
//   print(result)
// }

// "Ray on corner of box pointing along edge" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f))
//   val result = RaytracerGold.testIntersection(r, b)
//   print(result)
// }

// "Ray outside box pointing towards box" in {
//   val b = new SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
//   val r = new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f))
//   val result = RaytracerGold.testIntersection(r, b)
//   print(result)
// }

//   "Ray hits node 1 then node 2" in {}
//   "Ray hits node 4 then 1 then 2 then misses 3" in {}
//   "Ray outside box pointing along edge" in {}

}