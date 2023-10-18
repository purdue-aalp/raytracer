package baseline_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scala.math._
import scala.collection.mutable.BitSet
import chiseltest.experimental.expose
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import baseline_datapath.raytracer_gold._
import baseline_datapath.raytracer_gold.RaytracerTestHelper._ // implicit conversions
import baseline_datapath.raytracer_gold.SW_Ray.SCENE_BOUNDS

import chiseltest.internal.CachingAnnotation
import firrtl2.options.TargetDirAnnotation
import chisel3.stage.{
  PrintFullStackTraceAnnotation,
  ThrowOnFirstErrorAnnotation
}
import chiseltest.simulator.{
  VerilatorFlags,
  VerilatorCFlags,
  VerilatorLinkFlags,
  WriteVcdAnnotation,
  CachingDebugAnnotation
}

class Datapath_wrapper extends Datapath {
  import hardfloat._
  val exposed_time = expose(_time)
  // val exposed_tmin = expose(fNFromRecFN(8, 24, tmin))
  // val exposed_tmax = expose(fNFromRecFN(8, 24, tmax))
  // val exposed_aabb_3 = expose(AABBConvertRecFNtoFN(geometries_shift_reg(3).aabb))
  // val exposed_ray_2 = expose(RayConvertRecFNtoFN(geometries_shift_reg(2).ray))
  // val exposed_adder_exception_0 = expose(adder_exceptions(0))
  // val exposed_tmin_3d = expose(Float3ConvertRecFNtoFN(tmin_3d))
  // val exposed_tmax_3d = expose(Float3ConvertRecFNtoFN(tmax_3d))
}

class UnifiedDatapath_wrapper extends UnifiedDatapath {
  val exposed_time = expose(_time)
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester {

  // we are dealing with hardware and software Ray types
  type HW_Ray = baseline_datapath.Ray
  type HW_Box = baseline_datapath.AABB

  val r = new Random()
  val N_RANDOM_TEST = 1000
  val PRINT_END_TIME = false
  val float_tolerance_error =
    0.001 // normalized error: 149 vs 100 would have an error of 0.49

  val chisel_test_annotations = Seq(
    VerilatorBackendAnnotation,
    CachingAnnotation,
    // CachingDebugAnnotation,
    TargetDirAnnotation("cached_verilator_backend/Datapath"),
    // WriteVcdAnnotation,
    VerilatorCFlags(Seq("-O3", "-march=native")),
    VerilatorLinkFlags(Seq("-O3", "-march=native")),
    VerilatorFlags(Seq("-O3", "--threads", "8"))
  )
  val chisel_test_chisel_annotations = Seq(
    ThrowOnFirstErrorAnnotation,
    PrintFullStackTraceAnnotation
  )

  // Define a function for ray-box intersection testing
  def testRayBoxIntersection(
      description: String,
      box_seq_seq: Seq[Seq[SW_Box]],
      ray_seq: Seq[SW_Ray]
  ): Unit = {
    // Everything we pass to the DecoupledDriver are LazyList,
    // which means elements won't be evaluated until they are accessed.
    // Furhtermore, using `def` instead of `val` allows garbage collection
    // to destroy spent elements ASAP.

    def ray_box_list: LazyList[SW_CombinedData] = LazyList.from {
      (ray_seq zip box_seq_seq).map { case (r, bs) =>
        SW_CombinedData(r, bs, SW_Triangle(), false)
      }
    }

    // a sequence of software gold results
    def sw_result_seq: LazyList[RaytracerGold.SW_RayBox_Result] = {
      ray_box_list.map {
        case SW_CombinedData(r, bseq, t, _) => {
          // println("calculated a sw result")
          RaytracerGold.testIntersection(r, bseq)
        }
      }
    }

    var worst_normalized_error = 0.0f

    description in {
      test(new UnifiedDatapath_wrapper)
        .withAnnotations(
          chisel_test_annotations
        )
        .withChiselAnnotations(
          chisel_test_chisel_annotations
        ) { dut =>
          dut.in.initSource().setSourceClock(dut.clock)
          dut.out.initSink().setSinkClock(dut.clock)

          fork {
            // On the one hand, we pipe-in the test case inputs
            // (our helper method will automatically convert them from SW_Combined
            // to CombinedRayBoxTriangleBundle).
            dut.in.enqueueSeq(ray_box_list)
          }.fork {
            // On the other hand, we sit at the output side and examing for valid
            // outputs one-by-one.
            // Because non-intersects have unspecified order, the software result
            // may have different order than the actual hw output. Hence the for
            // loop.
            sw_result_seq.zipWithIndex.foreach { case (sw_r, input_no) =>
              dut.out.waitForValid()

              // val o = dut.out.bits.peek()
              //   println(s"actual tmin: ${o.tmin_out.map(bitsToFloat(_))}")
              //   println(s"actual intersect: ${o.isIntersect.map(_.litValue)}")
              //   println(s"actual boxidx: ${o.boxIndex.map(_.litValue)}")
              // println(s"Predicted: ${sw_r}")

              // create an immutable Map: from box_index to tuple (t_min, is_intersect)
              val input_box_status =
                Map(sw_r.t_min.zip(sw_r.is_intersect).zip(sw_r.box_index).map {
                  case ((t, isint), boxidx) =>
                    (boxidx, (t, isint))
                }: _*)

              // traverse the four elements of actual output, verify their
              // ordering is correct, and they match with what the software gold
              // result predicts
              val error_string_obj = new AnyRef {
                lazy val o = dut.out.bits.peek()
                override def toString: String = {
                  s"input #${input_no}\nactual tmin: ${o.tmin_out.map(bitsToFloat(_))}\n" + s"actual intersect: ${o.isIntersect
                      .map(_.litValue)}\n" + s"actual boxidx: ${o.boxIndex.map(_.litValue)}\n" + s"Predicted: ${sw_r}"
                }
              }

              var has_seen_non_intersect = false
              var so_far_largest_tmin = 0.0f
              for (idx <- 0 until 4) {
                val actual_tmin = bitsToFloat(dut.out.bits.tmin_out(idx).peek())
                val actual_is_intersect =
                  dut.out.bits.isIntersect(idx).peek().litToBoolean
                val actual_box_idx =
                  dut.out.bits.boxIndex(idx).peek().litValue.intValue

                // check t value monotonicity (use scala.Predef.assert instead of
                // scalatest.assert so we can call error_string by-name)
                assert(actual_tmin >= so_far_largest_tmin, error_string_obj)

                // check intersects go before non-intersects
                assert(
                  !has_seen_non_intersect || (has_seen_non_intersect && !actual_is_intersect),
                  error_string_obj
                )

                // checkout HW and SW yields the same opinion on intersection
                assert(
                  input_box_status(actual_box_idx)._2 == actual_is_intersect,
                  error_string_obj
                )

                // check HW and SW yields the same intersect t value for each
                // box, if intersection is true
                if (actual_is_intersect) {
                  val normalized_tmin_error = if (actual_tmin != 0.0f) {
                    abs(
                      input_box_status(actual_box_idx)._1 - actual_tmin
                    ) / actual_tmin
                  } else { input_box_status(actual_box_idx)._1 }
                  assert(
                    normalized_tmin_error <= float_tolerance_error,
                    error_string_obj
                  )
                  worst_normalized_error =
                    max(worst_normalized_error, normalized_tmin_error)
                }

                if (!has_seen_non_intersect && !actual_is_intersect) {
                  has_seen_non_intersect = true
                }

                so_far_largest_tmin = actual_tmin
              }

              // // a mutable BitSet tracks which input-box does not intersect
              // val predicted_non_intersect_boxes = new collection.mutable.BitSet(4)

              // //TODO: handle case when multiple intersect t values are equal!

              // //This part checks for boxes the SW predict to intersect
              // for(idx <- 0 until 4){
              //   if(sw_r.is_intersect(idx)){
              //     lazy val o = dut.out.bits.peek()
              //     lazy val error_string = s"input #${input_no}\nactual tmin: ${o.tmin_out.map(bitsToFloat(_))}\n" + s"actual intersect: ${o.isIntersect.map(_.litValue)}\n" +  s"actual boxidx: ${o.boxIndex.map(_.litValue)}\n" + s"Predicted: ${sw_r}"

              //     dut.out.bits.isIntersect(idx).expect(sw_r.is_intersect(idx).B, error_string)
              //     dut.out.bits.boxIndex(idx).expect((sw_r.box_index(idx).U), error_string)
              //     dut.out.bits.tmin_out(idx).expect(floatToBits(sw_r.t_min(idx)), error_string )
              //   }
              //   else{
              //     predicted_non_intersect_boxes.add(sw_r.box_index(idx))
              //   }
              // }
              // // This part checks for boxes the SW predict to not intersect
              // for(idx <- 0 until 4){
              //   val box_idx: Int = dut.out.bits.boxIndex(idx).peek().litValue.intValue
              //   if(dut.out.bits.isIntersect(idx).peek().litToBoolean){
              //     assert(!predicted_non_intersect_boxes.contains(box_idx))
              //   }
              //   else{
              //     assert(predicted_non_intersect_boxes.contains(box_idx))
              //   }
              // }

              dut.clock.step()
            }
          }.join()

          if (PRINT_END_TIME) {
            println(s"test ends at time ${dut.exposed_time.peek().litValue}")
          }

        }
      println(s"worst normalized error is ${worst_normalized_error}")
    }
  }

  def testRayTriangleIntersection(
      description: String,
      triangle_seq: Seq[SW_Triangle],
      ray_seq: Seq[SW_Ray]
  ): Unit = {
    description in {
      def ray_triangle_list: LazyList[SW_CombinedData] = LazyList.from {
        (ray_seq zip triangle_seq).map { case (r, ts) =>
          lazy val _four_empty_boxes = Seq.fill[SW_Box](4)(SW_Box())
          SW_CombinedData(r, _four_empty_boxes, ts, true)
        }
      }

      // a sequence of software gold results
      def sw_result_seq: LazyList[RaytracerGold.SW_RayTriangle_Result] = {
        ray_triangle_list.map {
          case SW_CombinedData(r, _, t, true) => {
            // println("calculated a sw result")
            RaytracerGold.testTriangleIntersection(r, t)
          }
          case _ => { throw new Exception("cannot take ray box data") }
        }
      }

      var worst_normalized_error = 0.0f

      test(new UnifiedDatapath_wrapper)
        .withAnnotations(
          chisel_test_annotations
        )
        .withChiselAnnotations(
          chisel_test_chisel_annotations
        ) { dut =>
          dut.in.initSource().setSourceClock(dut.clock)
          dut.out.initSink().setSinkClock(dut.clock)

          // ray_triangle_list.zip(sw_result_seq).foreach { case (data, result) =>
          //   println(f"data: ${data}, result: ${result}")
          // }
          fork {
            dut.in.enqueueSeq(ray_triangle_list)
          }.fork {
            sw_result_seq.zipWithIndex.zip(ray_triangle_list).foreach {
              case ((sw_r, input_no), input) =>
                dut.out.waitForValid()

                val hw_t_num: Float = bitsToFloat(dut.out.bits.t_num.peek())
                val hw_t_denom: Float = bitsToFloat(dut.out.bits.t_denom.peek())
                val hw_hit: Boolean =
                  dut.out.bits.triangle_hit.peek().litToBoolean

                val error_msg_obj = new AnyRef {
                  lazy val message =
                    f"Input #${input_no}: ${input},\n predicted output: ${sw_r}, \n actual output: t_num=${hw_t_num}, t_denom=${hw_t_denom}, hit=${hw_hit}\n"
                  override def toString: String = message
                }

                assert(hw_hit == sw_r.is_hit, error_msg_obj)
                if (sw_r.is_hit) {
                  val hw_t = hw_t_num / hw_t_denom
                  val sw_t = sw_r.t_num / sw_r.t_denom
                  val t_error = abs(hw_t-sw_t) / sw_t

                  // assert(t_error <= float_tolerance_error, error_msg_obj)
                  worst_normalized_error =
                    max(worst_normalized_error, t_error)
                }

                dut.clock.step()
            }
          }.join()

          if (PRINT_END_TIME) {
            println(s"test ends at time ${dut.exposed_time.peek().litValue}")
          }
        }
      println(s"worst normalized error is ${worst_normalized_error}")
    }
  }

  // Define test cases
  testRayBoxIntersection(
    "Small Ray inside box",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(
      float_3(0.5f, -0.5f, 0.5f),
      float_3(0.0001f, -0.0001f, -0.001f)
    ) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on edge of box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing along edge",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing towards box",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray hits node 1 then node 2",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
      SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f,
        0.5f) :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray hits node 4 then 1 then 2 then misses 3",
    (SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      SW_Box(100.0f, 100.25f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      SW_Box(-5.0f, -4.0f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
      Nil) :: Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray outside box pointing along edge",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
      1.0f) :: SW_Box() :: SW_Box() :: SW_Box() :: Nil) :: Nil,
    new SW_Ray(float_3(1.0f, 2.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits back of triangle along normal to surface",
    SW_Triangle(
      float_3(-1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits front of triangle along normal to surface",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -1.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits front edge of triangle along normal to surface",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(0.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits front corner of triangle along normal to surface",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 0.0f, -1.0f),
      float_3(0.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray misses triangle",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, 5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray misses triangle along normal to surface",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(-5.0f, 5.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits front of triangle along normal to surface (further away)",
    SW_Triangle(
      float_3(1.0f, -1.0f, -100.0f),
      float_3(0.0f, 1.0f, -100.0f),
      float_3(-1.0f, -1.0f, -100.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits front of triangle not along normal",
    SW_Triangle(
      float_3(1.0f, -1.0f, -3.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -2.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(0.0f, 0.0f, -5.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits edge of triangle perpendicular to normal",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(0.0f, -2.0f, -1.0f), float_3(0.0f, 5.0f, 0.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits triangle (normal on x-axis)",
    SW_Triangle(
      float_3(1.0f, -1.0f, 1.0f),
      float_3(1.0f, 1.0f, 0.0f),
      float_3(1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(float_3(-100.0f, 0.0f, 0.0f), float_3(0.25f, 0.0f, 0.0f)) :: Nil
  )

  testRayTriangleIntersection(
    "Ray hits edge of triangle perpendicular to normal from inside tri",
    SW_Triangle(
      float_3(1.0f, -1.0f, -1.0f),
      float_3(0.0f, 1.0f, -1.0f),
      float_3(-1.0f, -1.0f, -1.0f)
    ) :: Nil,
    new SW_Ray(
      float_3(0.0f, -1.0f / 3, -1.0f),
      float_3(0.0f, 5.0f, 0.0f)
    ) :: Nil
  )

  // randomized tests
  testRayBoxIntersection(
    s"${N_RANDOM_TEST} randomized rays and boxes within range -10000.0, 10000.0",
    List.fill(N_RANDOM_TEST) {
      List.fill(4) { RaytracerGold.genRandomBox(1e16.toFloat) }
    },
    List.fill(N_RANDOM_TEST) { RaytracerGold.genRandomRay(1e5.toFloat) }
  )

  val randTriangle = List.fill(N_RANDOM_TEST)(
    RaytracerGold.genRandomTriangle(-SCENE_BOUNDS.toFloat, SCENE_BOUNDS.toFloat)
  )
  testRayTriangleIntersection(
    s"${N_RANDOM_TEST} randomized rays and triangles within range ${SCENE_BOUNDS}",
    randTriangle,
    randTriangle.map { t =>
      RaytracerGold.genRandomRayGivenPoint(
        t.centroid,
        -SCENE_BOUNDS.toFloat,
        SCENE_BOUNDS.toFloat
      )
    }
  )

}
