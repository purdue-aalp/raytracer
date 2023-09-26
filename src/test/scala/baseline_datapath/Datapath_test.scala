package baseline_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scala.math._
import chiseltest.experimental.expose
import chisel3.experimental.BundleLiterals._

import baseline_datapath.raytracer_gold._
import baseline_datapath.raytracer_gold.RaytracerTestHelper._ // implicit conversions

import chiseltest.internal.CachingAnnotation
import firrtl2.options.TargetDirAnnotation
import chisel3.stage.{PrintFullStackTraceAnnotation,ThrowOnFirstErrorAnnotation}
import chiseltest.simulator.{VerilatorCFlags, VerilatorLinkFlags, WriteVcdAnnotation, CachingDebugAnnotation}

class Datapath_wrapper extends Datapath {
  import hardfloat._
  val exposed_time = expose(_time)
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
          // CachingDebugAnnotation,
          TargetDirAnnotation("cached_verilator_backend/Datapath"),
          // WriteVcdAnnotation,
          VerilatorCFlags(Seq("-O3")),
          VerilatorLinkFlags(Seq("-O3"))
        )
      ).withChiselAnnotations(
        Seq(
          ThrowOnFirstErrorAnnotation
        )
      ) { dut =>
        dut.in.initSource().setSourceClock(dut.clock)
        dut.out.initSink().setSinkClock(dut.clock)

        // Everything we pass to the DecoupledDriver are LazyList, 
        // which means elements won't be evaluated until they are accessed.
        // Furhtermore, using `def` instead of `val` allows garbage collection
        // to destroy spent elements ASAP.

        // cartesian product of all boxes and rays
        def ray_box_list = LazyList.from{
          for(ray <- ray_seq; box <- box_seq) yield (ray, box)
        }
        
        // a sequence of software gold results
        def sw_result_seq : LazyList[Option[Float]] = {
          ray_box_list.map{
            case(r, b) => {
              // println("calculated a sw result")
              RaytracerGold.testIntersection(r, b)
            }
          }
        }

        // hardware-format result, of the type 
        // Bundle{val tmin_out: Bits, val isIntersect: Bool}
        def expectedResult = sw_result_seq.map{x => 
          chiselTypeOf(dut.out.bits).Lit(
            _.tmin_out -> x.map(floatToBits(_)).getOrElse(floatToBits(-1.0f)),
            _.isIntersect -> x.nonEmpty.B
          )
        }

        fork{
          dut.in.enqueueSeq(ray_box_list)
        }.fork{
          dut.out.expectDequeueSeq(expectedResult)
        }.join()

        println(s"test ends at time ${dut.exposed_time.peek().litValue}")
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
