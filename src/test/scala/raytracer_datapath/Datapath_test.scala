package raytracer_datapath

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random
import scala.math._
import scala.collection.mutable.BitSet
import chiseltest.experimental.expose
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import raytracer_datapath.raytracer_gold._
import raytracer_datapath.raytracer_gold.RaytracerTestHelper._ // implicit conversions
import raytracer_datapath.raytracer_gold.SW_Ray.SCENE_BOUNDS

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
trait WithExposedTime extends Module {
  def exposed_time: Bits
}

class UnifiedDatapath_wrapper
    extends UnifiedDatapath(p = RaytracerParams(false, true, None))
    with WithExposedTime {
  val exposed_time = expose(_time)
}

class UnifiedDatapath_wrapper_16
    extends UnifiedDatapath(p = RaytracerParams(false, true, Some(16)))
    with WithExposedTime {
  import hardfloat._
  val exposed_time = expose(_time)
}


class Datapath_test extends AnyFreeSpec with ChiselScalatestTester {
  
/**
  * Contents:
    - Knobs for simulation
    - Necessary definitions
    - Testbench Routines
    - Helper Routines for testbench
  */

  ///////////////////////////
  // Knobs for simulation  //
  ///////////////////////////
  
  // Not enabling euclidean/angular mode
  val test_baseline_ray_box_random = true
  val test_baseline_ray_triangle_random = true

  // Enables euclidean/angular mode
  val test_extended_euclidean_random = true
  val test_extended_angular_random = true
  val test_extended_ray_box_random = true
  val test_extended_ray_triangle_random = true

  // Random test count
  val N_RANDOM_TEST = 100

  // Print how many cycles had been simulated after each test
  val PRINT_END_TIME = true

  // We don't want the FP results to deviate more than this much from the SW
  // "gold" results
  val float_tolerance_error =
    0.001 // normalized error: 149 vs 100 would have an error of 0.49
  
  // Dump a vcd file for unified tests. 
  // If you expect to get one vcd file for each test, better comment-out the
  // TargetDirAnnotation setting below, or only run one test at a time. Otherwise, tests that use the same
  // Verilator model will overwrite the vcd file of previous tests. 
  val dump_vcd_for_unified_test = false

  // More configurations for the test
  def chisel_test_annotations(description: String) = Seq(
    // Use Verilator as simulation backend. Make sure it's in PATH!
    VerilatorBackendAnnotation, 

    // Don't regenerate the same Verilator model for each test
    CachingAnnotation,

    // Print debug info for caching
    // CachingDebugAnnotation,

    // Where to cache the Verilator model
    TargetDirAnnotation(
      s"cached_verilator_backend/${description.replaceAll("[^0-9a-zA-Z]", "_")}"
    ),
    
    // To get better performance from the Verilator model
    VerilatorCFlags(Seq("-Os", "-march=native")),
    VerilatorLinkFlags(Seq("-Os", "-march=native")),
    VerilatorFlags(Seq("-O3", "--threads", "16"))
  )
  val chisel_test_chisel_annotations = Seq(
    ThrowOnFirstErrorAnnotation,
    PrintFullStackTraceAnnotation
  )

  //////////////////////////////
  // Necessary definitions    //
  //////////////////////////////

  // we are dealing with hardware and software Ray types
  type HW_Ray = raytracer_datapath.Ray
  type HW_Box = raytracer_datapath.AABB

  val r = new Random()

  val default_vec_a = SW_Vector((0 until 16).toList.map(_.toFloat))
  val default_vec_b = SW_Vector((16 until 0 by -1).toList.map(_.toFloat))
  val empty_vec_a = SW_Vector(Nil)
  val empty_vec_b = SW_Vector(Nil)

  //////////////////
  // Test suites  //
  //////////////////

  // randomized tests
  val box_seq_for_raybox = List.fill(N_RANDOM_TEST) {
    List.fill(4) { RandomSWData.genRandomBox(1e4.toFloat, r) }
  }
  val ray_seq_for_raybox = List.fill(N_RANDOM_TEST) {
    RandomSWData.genRandomRay(1e2.toFloat, r)
  }

  if (test_baseline_ray_box_random) {
    testUnifiedIntersection(
      false,
      s"baseline ray box random test ${N_RANDOM_TEST}",
      ray_seq_for_raybox,
      box_seq_for_raybox,
      Seq.fill(ray_seq_for_raybox.length)(
        SW_Triangle()
      ),
      Seq.fill(ray_seq_for_raybox.length)(SW_OpQuadbox)
    )
  }

  val tri_seq_for_raytriangle = List.fill(N_RANDOM_TEST)(
    RandomSWData.genRandomTriangle(
      -SCENE_BOUNDS.toFloat,
      SCENE_BOUNDS.toFloat,
      r
    )
  )
  val ray_seq_for_raytriangle = tri_seq_for_raytriangle.map { t =>
    RandomSWData.genRandomRayGivenPoint(
      t.centroid,
      -SCENE_BOUNDS.toFloat,
      SCENE_BOUNDS.toFloat,
      r
    )
  }

  if (test_baseline_ray_triangle_random) {
    testUnifiedIntersection(
      false,
      s"baseline ray triangle random test ${N_RANDOM_TEST}",
      ray_seq_for_raytriangle,
      Seq.fill(ray_seq_for_raytriangle.length)(
        Seq.fill(4)(SW_Box())
      ),
      tri_seq_for_raytriangle,
      Seq.fill(ray_seq_for_raybox.length)(SW_OpTriangle)
    )
  }

  if (test_extended_ray_box_random) {
    testUnifiedIntersection(
      true,
      s"extended ray box random test ${N_RANDOM_TEST}",
      ray_seq_for_raybox,
      box_seq_for_raybox,
      Seq.fill(ray_seq_for_raybox.length)(
        SW_Triangle()
      ),
      Seq.fill(ray_seq_for_raybox.length)(SW_OpQuadbox)
    )
  }

  if (test_extended_ray_triangle_random) {
    testUnifiedIntersection(
      true,
      s"extended ray triangle random test ${N_RANDOM_TEST}",
      ray_seq_for_raytriangle,
      Seq.fill(ray_seq_for_raytriangle.length)(
        Seq.fill(4)(SW_Box())
      ),
      tri_seq_for_raytriangle,
      Seq.fill(
        ray_seq_for_raytriangle.length
      )(SW_OpTriangle)
    )
  }

  val vec_pair_seq =
    Seq.fill(N_RANDOM_TEST)(
      RandomSWData.genRandomVectorPair(-10000.0f, 10000.0f, 16 * 8, r)
    )

  val vec_a = vec_pair_seq.map { case (_1, _2) => _1 }
  val vec_b = vec_pair_seq.map { case (_1, _2) => _2 }

  if (test_extended_euclidean_random) {
    testEuclidean(
      s"extended euclidean test ${N_RANDOM_TEST}",
      vec_a,
      vec_b
    )
  }

  if (test_extended_angular_random) {
    testAngular(
      s"extended angular test ${N_RANDOM_TEST}",
      vec_a,
      vec_b
    )
  }


  ///////////////////////////
  // Testbench Routines    //
  ///////////////////////////

  def testEuclidean(
      description: String,
      seq_vec_a: Seq[SW_Vector],
      seq_vec_b: Seq[SW_Vector]
  ): Unit = description in {
    val combined_data_list: List[SW_EnhancedCombinedData] = List.from {
      (seq_vec_a zip seq_vec_b).flatMap { case (a, b) =>
        get_euclidean_job_seq_from_vec_pair(a, b)
      }
    }

    val result_list = (seq_vec_a zip seq_vec_b).map { case (va, vb) =>
      va.calc_diff(vb)
    }

    var worst_normalized_diff = 0.0f

    test(new UnifiedDatapath_wrapper_16)
      .withAnnotations(
        chisel_test_annotations("euclidean"+true.toString()) :++ {
          if (dump_vcd_for_unified_test) { WriteVcdAnnotation :: Nil }
          else { Nil }
        }
        // chisel_test_annotations(description)
      )
      .withChiselAnnotations(
        chisel_test_chisel_annotations
      ) { dut =>
        dut.in.initSource().setSourceClock(dut.clock)
        dut.out.initSink().setSinkClock(dut.clock)

        fork {
          dut.in.enqueueSeq(combined_data_list)
        }.fork {
          dut.out.ready.poke(true.B)
          result_list.zipWithIndex.foreach { case (sw_sum, idx) =>
            dut.out.waitForValid()

            // only inspect the hw output on the last beat, i.e. those that
            // asserts the reset_accum signal
            while (!dut.out.bits.euclidean_reset_accum.peek()(0).litToBoolean) {
              dut.clock.step()
              dut.out.waitForValid()
            }

            val normalized_diff = abs(
              sw_sum - bitsToFloat(
                dut.out.bits.euclidean_accumulator(0).peek()
              )
            ) / sw_sum
            if (normalized_diff > worst_normalized_diff)
              worst_normalized_diff = normalized_diff

            dut.clock.step()
          }
        }.join()

        println(s"worse normalzied diff is ${worst_normalized_diff}")
        if (PRINT_END_TIME) {
          println(s"test ends at time ${dut.clock.getStepCount}")
        }
      }
  }

  def testAngular(
      description: String,
      seq_vec_a: Seq[SW_Vector],
      seq_vec_b: Seq[SW_Vector]
  ): Unit = description in {
    val combined_data_list: List[SW_EnhancedCombinedData] = List.from {
      (seq_vec_a zip seq_vec_b).flatMap { case (a, b) =>
        get_angular_job_seq_from_vec_pair(a, b)
      }
    }

    val result_list = (seq_vec_a zip seq_vec_b).map { case (query, candidate) =>
      Map(
        "dot_product" -> query.calc_dot_product(candidate),
        "norm" -> candidate.get_norm()
      )
    }

    var worst_normalized_diff = 0.0f

    test(new UnifiedDatapath_wrapper_16)
      .withAnnotations(
        chisel_test_annotations("euclidean"+true.toString()) :++ {
          if (dump_vcd_for_unified_test) { WriteVcdAnnotation :: Nil }
          else { Nil }
        }
      )
      .withChiselAnnotations(
        chisel_test_chisel_annotations
      ) { dut =>
        dut.in.initSource().setSourceClock(dut.clock)
        dut.out.initSink().setSinkClock(dut.clock)

        fork {
          dut.in.enqueueSeq(combined_data_list)
        }.fork {
          dut.out.ready.poke(true.B)
          result_list.zipWithIndex.foreach { case (sw_result, idx) =>
            dut.out.waitForValid()

            // only inspect the hw output on the last beat, i.e. those that
            // asserts the reset_accum signal
            while (!dut.out.bits.angular_reset_accum.peek()(0).litToBoolean) {
              dut.clock.step()
              dut.out.waitForValid()
            }

            val normalized_diff_dot_product = abs(
              sw_result("dot_product") - bitsToFloat(
                dut.out.bits.angular_dot_product(0).peek()
              )
            ) / sw_result("dot_product")
            val normalized_diff_norm = abs(
              sw_result("norm") - bitsToFloat(
                dut.out.bits.angular_norm(0).peek()
              )
            ) / sw_result("norm")
            if (normalized_diff_dot_product > worst_normalized_diff)
              worst_normalized_diff = normalized_diff_dot_product
            if (normalized_diff_norm > worst_normalized_diff)
              worst_normalized_diff = normalized_diff_norm

            dut.clock.step()
          }
        }.join()

        println(s"worse normalzied diff is ${worst_normalized_diff}")
        if (PRINT_END_TIME) {
          println(s"test ends at time ${dut.clock.getStepCount}")
        }
      }
  }

  def testUnifiedIntersection(
      extended: Boolean,
      description: String,
      ray_seq: Seq[SW_Ray],
      box_seq_seq: Seq[Seq[SW_Box]],
      triangle_seq: Seq[SW_Triangle],
      op_seq: Seq[SW_Opcode]
  ): Unit = {
    description in {
      val combined_data_list: List[SW_EnhancedCombinedData] = List.from {
        extended match {
          case true =>
            // input bundle should include 16-elements for each of vec a and b
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
                  true,
                  0
                )
            }
          case false =>
            // input bundle does not include vector elements
            (ray_seq zip box_seq_seq zip triangle_seq zip op_seq).map {
              case (((ray, boxs), tri), op) =>
                SW_EnhancedCombinedData(
                  ray,
                  boxs,
                  tri,
                  op,
                  None,
                  empty_vec_a,
                  empty_vec_b,
                  true,
                  0
                )
            }
        }

      }

      // a sequence of software gold results
      val sw_result_seq: List[SW_Unified_Result] = {
        combined_data_list.map {
          case SW_EnhancedCombinedData(
                r,
                _,
                t,
                SW_OpTriangle,
                _,
                _,
                _,
                _,
                _
              ) => {
            // println("calculated a sw result")
            SW_Unified_Result(
              true,
              RaytracerGold.testTriangleIntersection(r, t),
              SW_RayBox_Result()
            )
          }
          case SW_EnhancedCombinedData(r, bs, _, SW_OpQuadbox, _, _, _, _, _) =>
            SW_Unified_Result(
              false,
              SW_RayTriangle_Result(),
              RaytracerGold.testIntersection(r, bs)
            )
          case _ => {
            throw new Exception("cannot take jobs other than box/triangle")
          }
        }
      }

      var worst_normalized_error = 0.0f

      test(gen_baseline_or_extended_datapath(extended))
        .withAnnotations(
          chisel_test_annotations("euclidean"+extended.toString()) :++ {
            if (dump_vcd_for_unified_test) { WriteVcdAnnotation :: Nil }
            else { Nil }
          }
        )
        .withChiselAnnotations(
          chisel_test_chisel_annotations
        ) { dut =>
          dut.in.initSource().setSourceClock(dut.clock)
          dut.out.initSink().setSinkClock(dut.clock)

          fork {
            dut.in.enqueueSeq(combined_data_list)
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


  //////////////////////////////////////
  // Helper Routines for testbench    //
  //////////////////////////////////////

  def gen_baseline_or_extended_datapath(extended: Boolean) = extended match {
    case true  => new UnifiedDatapath_wrapper_16
    case false => new UnifiedDatapath_wrapper
  }
  
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
    }

    t_error
  }
}

