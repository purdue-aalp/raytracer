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

// SW opcode is an enumeration defined in SW_Data.scala
import SW_Opcode._

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

class UnifiedDatapath_wrapper
    extends UnifiedDatapath(p = RaytracerParams(false, true, None)) {
  val exposed_time = expose(_time)
}

class UnifiedDatapath_wrapper_16
    extends UnifiedDatapath(p = RaytracerParams(false, true, Some(16))) {
  val exposed_time = expose(_time)
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester {

  // we are dealing with hardware and software Ray types
  type HW_Ray = baseline_datapath.Ray
  type HW_Box = baseline_datapath.AABB

  val r = new Random()
  val N_RANDOM_TEST = 500
  val PRINT_END_TIME = true
  val float_tolerance_error =
    0.001 // normalized error: 149 vs 100 would have an error of 0.49
  val use_stage_submodule = false
  val dump_vcd_for_unified_test = false
  val test_ray_box_specific = false
  val test_ray_triangle_specific = false
  val test_ray_box_random = true
  val test_ray_triangle_random = true
  val test_unified_random = true

  val default_vec_a = SW_Vector((0 until 16).toList.map(_.toFloat))
  val default_vec_b = SW_Vector((16 until 0 by -1).toList.map(_.toFloat))
  val empty_vec_a = SW_Vector(Nil)
  val empty_vec_b = SW_Vector(Nil)

  val chisel_test_annotations = Seq(
    VerilatorBackendAnnotation,
    CachingAnnotation,
    // CachingDebugAnnotation,
    // TargetDirAnnotation(
    //   if (use_stage_submodule)
    //     "cached_verilator_backend/Datapath_stage_submodule"
    //   else "cached_verilator_backend/Datapath_monolithic"
    // ),
    // WriteVcdAnnotation,
    VerilatorCFlags(Seq("-O3", "-march=native")),
    VerilatorLinkFlags(Seq("-O3", "-march=native")),
    VerilatorFlags(Seq("-O3", "--threads", "16"))
  )
  val chisel_test_chisel_annotations = Seq(
    ThrowOnFirstErrorAnnotation,
    PrintFullStackTraceAnnotation
  )

  def check_raybox_result(
      input_no: Int,
      sw_result: SW_RayBox_Result,
      hw_result: UnifiedDatapathOutput
  ) = {
    val input_box_status =
      Map(
        sw_result.t_min
          .zip(sw_result.is_intersect)
          .zip(sw_result.box_index)
          .map { case ((t, isint), boxidx) =>
            (boxidx, (t, isint))
          }: _*
      )

    // traverse the four elements of actual output, verify their
    // ordering is correct, and they match with what the software gold
    // result predicts
    val error_string_obj = new AnyRef {
      lazy val o = hw_result.peek()
      override def toString: String = {
        s"input #${input_no}\nactual tmin: ${o.tmin_out.map(bitsToFloat(_))}\n" + s"actual intersect: ${o.isIntersect
            .map(_.litValue)}\n" + s"actual boxidx: ${o.boxIndex.map(_.litValue)}\n" + s"Predicted: ${sw_result}"
      }
    }

    var has_seen_non_intersect = false
    var so_far_largest_tmin = 0.0f
    for (idx <- 0 until 4) {
      val actual_tmin = bitsToFloat(hw_result.tmin_out(idx).peek())
      val actual_is_intersect =
        hw_result.isIntersect(idx).peek().litToBoolean
      val actual_box_idx =
        hw_result.boxIndex(idx).peek().litValue.intValue

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
      }

      if (!has_seen_non_intersect && !actual_is_intersect) {
        has_seen_non_intersect = true
      }

      so_far_largest_tmin = actual_tmin
    }

  }

  def check_raytriangle_result(
      input_no: Int,
      sw_result: SW_RayTriangle_Result,
      hw_result: UnifiedDatapathOutput
  ): Float = {
    val hw_t_num: Float = bitsToFloat(hw_result.t_num.peek())
    val hw_t_denom: Float = bitsToFloat(hw_result.t_denom.peek())
    val hw_hit: Boolean =
      hw_result.triangle_hit.peek().litToBoolean

    val error_msg_obj = new AnyRef {
      lazy val message =
        f"Input #${input_no}: ${input_no},\n predicted output: ${sw_result}, \n actual output: t_num=${hw_t_num}, t_denom=${hw_t_denom}, hit=${hw_hit}\n"
      override def toString: String = message
    }

    var t_error = 0.0f
    assert(hw_hit == sw_result.is_hit, error_msg_obj)
    if (sw_result.is_hit) {
      val hw_t = hw_t_num / hw_t_denom
      val sw_t = sw_result.t_num / sw_result.t_denom
      t_error = abs(hw_t - sw_t) / sw_t

      // assert(t_error <= float_tolerance_error, error_msg_obj)
      // worst_normalized_error = max(worst_normalized_error, t_error)
    }

    t_error
  }

  // Define a function for ray-box intersection testing
  def testRayBoxIntersection(
      description: String,
      box_seq_seq: Seq[Seq[SW_Box]],
      ray_seq: Seq[SW_Ray]
  ): Unit = {

    val ray_box_list: List[SW_EnhancedCombinedData] = List.from {
      (ray_seq zip box_seq_seq).map { case (r, bs) =>
        SW_EnhancedCombinedData(
          r,
          bs,
          SW_Triangle(),
          SW_OpQuadbox,
          None,
          empty_vec_a,
          empty_vec_b,
          true
        )
      }
    }

    // a sequence of software gold results
    val sw_result_seq: List[SW_RayBox_Result] = {
      ray_box_list.map {
        case SW_EnhancedCombinedData(r, bseq, t, SW_OpQuadbox, _, _, _, _) => {
          // println("calculated a sw result")
          RaytracerGold.testIntersection(r, bseq)
        }
        case _ => throw new Exception("must be ray-box op!")
      }
    }

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
            dut.out.ready.poke(true.B)
            // On the other hand, we sit at the output side and examing for valid
            // outputs one-by-one.
            // Because non-intersects have unspecified order, the software result
            // may have different order than the actual hw output. Hence the for
            // loop.
            sw_result_seq.zipWithIndex.foreach { case (sw_r, input_no) =>
              dut.out.waitForValid()

              check_raybox_result(input_no, sw_r, dut.out.bits)

              dut.clock.step()
            }
          }.join()

          if (PRINT_END_TIME) {
            println(s"test ends at time ${dut.exposed_time.peek().litValue}")
          }

        }
    }
  }

  def testRayTriangleIntersection(
      description: String,
      triangle_seq: Seq[SW_Triangle],
      ray_seq: Seq[SW_Ray]
  ): Unit = {
    description in {
      val ray_triangle_list: List[SW_EnhancedCombinedData] = List.from {
        (ray_seq zip triangle_seq).map { case (r, ts) =>
          lazy val _four_empty_boxes = Seq.fill[SW_Box](4)(SW_Box())
          SW_EnhancedCombinedData(
            r,
            _four_empty_boxes,
            ts,
            SW_OpTriangle,
            None,
            empty_vec_a,
            empty_vec_b,
            true
          )
        }
      }

      // a sequence of software gold results
      val sw_result_seq: List[SW_RayTriangle_Result] = {
        ray_triangle_list.map {
          case SW_EnhancedCombinedData(r, _, t, SW_OpTriangle, _, _, _, _) => {
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
            dut.out.ready.poke(true.B)
            sw_result_seq.zipWithIndex.zip(ray_triangle_list).foreach {
              case ((sw_r, input_no), input) =>
                dut.out.waitForValid()

                // val hw_t_num: Float = bitsToFloat(dut.out.bits.t_num.peek())
                // val hw_t_denom: Float = bitsToFloat(dut.out.bits.t_denom.peek())
                // val hw_hit: Boolean =
                //   dut.out.bits.triangle_hit.peek().litToBoolean

                // val error_msg_obj = new AnyRef {
                //   lazy val message =
                //     f"Input #${input_no}: ${input},\n predicted output: ${sw_r}, \n actual output: t_num=${hw_t_num}, t_denom=${hw_t_denom}, hit=${hw_hit}\n"
                //   override def toString: String = message
                // }

                // assert(hw_hit == sw_r.is_hit, error_msg_obj)
                // if (sw_r.is_hit) {
                //   val hw_t = hw_t_num / hw_t_denom
                //   val sw_t = sw_r.t_num / sw_r.t_denom
                //   val t_error = abs(hw_t - sw_t) / sw_t

                //   // assert(t_error <= float_tolerance_error, error_msg_obj)
                //   worst_normalized_error = max(worst_normalized_error, t_error)
                // }

                val t_error =
                  check_raytriangle_result(input_no, sw_r, dut.out.bits)
                worst_normalized_error = max(worst_normalized_error, t_error)

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

  def testEuclidean(
      description: String,
      vec_a: Seq[SW_Vector],
      vec_b: Seq[SW_Vector]
  ): Unit = description in {
    val combined_data_list: List[SW_EnhancedCombinedData] = List.from {
      (vec_a zip vec_b).map { case (a, b) =>
        SW_EnhancedCombinedData(
          SW_Ray(float_3(0.0f, 0.0f, 0.0f), float_3(1.0f, 1.0f, 1.0f)),
          Seq.fill(4)(SW_Box()),
          SW_Triangle(),
          SW_OpEuclidean,
          Some(a.dim),
          a,
          b,
          true
        )
      }
    }
  }

  def testUnifiedIntersection(
      description: String,
      ray_seq: Seq[SW_Ray],
      box_seq_seq: Seq[Seq[SW_Box]],
      triangle_seq: Seq[SW_Triangle],
      op_seq: Seq[SW_Opcode]
  ): Unit = {
    description in {
      val combined_data_list: List[SW_EnhancedCombinedData] = List.from {
        (ray_seq zip box_seq_seq zip triangle_seq zip op_seq).map {
          case (((ray, boxs), tri), op) =>
            SW_EnhancedCombinedData(
              ray,
              boxs,
              tri,
              op,
              Some(16),
              default_vec_a,
              default_vec_b,
              true
            )
        }
      }

      // a sequence of software gold results
      val sw_result_seq: List[SW_Unified_Result] = {
        combined_data_list.map {
          case SW_EnhancedCombinedData(r, _, t, SW_OpTriangle, _, _, _, _) => {
            // println("calculated a sw result")
            SW_Unified_Result(
              true,
              RaytracerGold.testTriangleIntersection(r, t),
              SW_RayBox_Result()
            )
          }
          case SW_EnhancedCombinedData(r, bs, _, SW_OpQuadbox, _, _, _, _) =>
            SW_Unified_Result(
              false,
              SW_RayTriangle_Result(),
              RaytracerGold.testIntersection(r, bs)
            )
          case _ => { throw new Exception("cannot take ray box data") }
        }
      }

      var worst_normalized_error = 0.0f

      test(new UnifiedDatapath_wrapper_16)
        .withAnnotations(
          chisel_test_annotations :++ {
            if (dump_vcd_for_unified_test) { WriteVcdAnnotation :: Nil }
            else { Nil }
          }
          // chisel_test_annotations
        )
        .withChiselAnnotations(
          chisel_test_chisel_annotations
        ) { dut =>
          dut.in.initSource().setSourceClock(dut.clock)
          dut.out.initSink().setSinkClock(dut.clock)

          fork {
            dut.in.enqueueSeq(combined_data_list.toList)
          }.fork {
            dut.out.ready.poke(true.B)
            sw_result_seq.zipWithIndex.map { case (sw_r, input_no) =>
              dut.out.waitForValid()
              if (sw_r.isTriangle) {
                check_raytriangle_result(
                  input_no,
                  sw_r.triangle_result,
                  dut.out.bits
                )
              } else {
                check_raybox_result(input_no, sw_r.box_result, dut.out.bits)
              }
              dut.clock.step()
            }
          }.join()

          println(s"test ends at time ${dut.exposed_time.peek().litValue}")
        }

    }
  }

  // Define test cases
  if (test_ray_box_specific) {
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
  }

  if (test_ray_triangle_specific) {
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
      new SW_Ray(
        float_3(-100.0f, 0.0f, 0.0f),
        float_3(0.25f, 0.0f, 0.0f)
      ) :: Nil
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
  }

  // randomized tests
  val box_seq_for_raybox = List.fill(N_RANDOM_TEST) {
    List.fill(4) { RandomSWData.genRandomBox(1e16.toFloat) }
  }
  val ray_seq_for_raybox = List.fill(N_RANDOM_TEST) {
    RandomSWData.genRandomRay(1e5.toFloat)
  }

  if (test_ray_box_random) {
    testRayBoxIntersection(
      s"${N_RANDOM_TEST} randomized rays and boxes within range -10000.0, 10000.0",
      box_seq_for_raybox,
      ray_seq_for_raybox
    )
  }

  val tri_seq_for_raytriangle = List.fill(N_RANDOM_TEST)(
    RandomSWData.genRandomTriangle(-SCENE_BOUNDS.toFloat, SCENE_BOUNDS.toFloat)
  )
  val ray_seq_for_raytriangle = tri_seq_for_raytriangle.map { t =>
    RandomSWData.genRandomRayGivenPoint(
      t.centroid,
      -SCENE_BOUNDS.toFloat,
      SCENE_BOUNDS.toFloat
    )
  }

  if (test_ray_triangle_random) {
    testRayTriangleIntersection(
      s"${N_RANDOM_TEST} randomized rays and triangles within range ${SCENE_BOUNDS}",
      tri_seq_for_raytriangle,
      ray_seq_for_raytriangle
    )
  }

  if (test_unified_random) {
    testUnifiedIntersection(
      "unified intersection test",
      ray_seq_for_raybox :++ ray_seq_for_raytriangle,
      box_seq_for_raybox :++ Seq.fill(ray_seq_for_raytriangle.length)(
        Seq.fill(4)(SW_Box())
      ),
      Seq.fill(ray_seq_for_raybox.length)(
        SW_Triangle()
      ) :++ tri_seq_for_raytriangle,
      Seq.fill(ray_seq_for_raybox.length)(SW_OpQuadbox) :++ Seq.fill(
        ray_seq_for_raytriangle.length
      )(SW_OpTriangle)
    )
  }

}
