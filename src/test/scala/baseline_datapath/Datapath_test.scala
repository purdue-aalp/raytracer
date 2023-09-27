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

import chiseltest.internal.CachingAnnotation
import firrtl2.options.TargetDirAnnotation
import chisel3.stage.{PrintFullStackTraceAnnotation,ThrowOnFirstErrorAnnotation}
import chiseltest.simulator.{VerilatorFlags, VerilatorCFlags, VerilatorLinkFlags, WriteVcdAnnotation, CachingDebugAnnotation}

class Datapath_wrapper extends Datapath {
  import hardfloat._
  val exposed_time = expose(_time)
  // val exposed_tmin = expose(fNFromRecFN(8, 24, tmin))
  // val exposed_tmax = expose(fNFromRecFN(8, 24, tmax))
  // val exposed_aabb_3 = expose(AABBConvertRecFNtoFN(geometries_shift_reg(3).aabb))
  val exposed_ray_2 = expose(RayConvertRecFNtoFN(geometries_shift_reg(2).ray))
  val exposed_adder_exception_0 = expose(adder_exceptions(0))
  // val exposed_tmin_3d = expose(Float3ConvertRecFNtoFN(tmin_3d))
  // val exposed_tmax_3d = expose(Float3ConvertRecFNtoFN(tmax_3d))
}

class Datapath_test extends AnyFreeSpec with ChiselScalatestTester {

  // we are dealing with hardware and software Ray types
  type HW_Ray = baseline_datapath.Ray
  type HW_Box = baseline_datapath.AABB

  val r = new Random()
  val N_RANDOM_TEST = 1000000

  // Define a function for ray-box intersection testing
  def testRayBoxIntersection(
      description: String,
      box_seq_seq: Seq[Seq[SW_Box]],
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
          VerilatorCFlags(Seq("-O3", "-march=native")),
          VerilatorLinkFlags(Seq("-O3", "-march=native")),
          VerilatorFlags(Seq("-O3", "--threads","4")),
        )
      ).withChiselAnnotations(
        Seq(
          ThrowOnFirstErrorAnnotation,
          PrintFullStackTraceAnnotation
        )
      ) { dut =>
        dut.in.initSource().setSourceClock(dut.clock)
        dut.out.initSink().setSinkClock(dut.clock)

        // Everything we pass to the DecoupledDriver are LazyList, 
        // which means elements won't be evaluated until they are accessed.
        // Furhtermore, using `def` instead of `val` allows garbage collection
        // to destroy spent elements ASAP.

        def ray_box_list: LazyList[SW_CombinedData] = LazyList.from{
          (ray_seq zip box_seq_seq).map{case(r, bs)=>SW_CombinedData(r, bs, false)}
        }
        
        // a sequence of software gold results
        def sw_result_seq : LazyList[RaytracerGold.SW_RayBox_Result] = {
          ray_box_list.map{
            case SW_CombinedData(r, bseq, _) => {
              // println("calculated a sw result")
              RaytracerGold.testIntersection(r, bseq)
            }
          }
        }

        fork{
          // On the one hand, we pipe-in the test case inputs
          // (our helper method will automatically convert them from SW_Combined
          // to CombinedRayBoxTriangleBundle).
          dut.in.enqueueSeq(ray_box_list)
        }.fork{
          // On the other hand, we sit at the output side and examing for valid
          // outputs one-by-one. 
          // Because non-intersects have unspecified order, the software result
          // may have different order than the actual hw output. Hence the for
          // loop. 
          sw_result_seq.zipWithIndex.foreach{case(sw_r, input_no) => 
            dut.out.waitForValid()

            // val o = dut.out.bits.peek()
            //   println(s"actual tmin: ${o.tmin_out.map(bitsToFloat(_))}")
            //   println(s"actual intersect: ${o.isIntersect.map(_.litValue)}")
            //   println(s"actual boxidx: ${o.boxIndex.map(_.litValue)}")
            // println(s"Predicted: ${sw_r}")

            // create an immutable Map: from box_index to tuple (t_min, is_intersect)
            val input_box_status = Map(sw_r.t_min.zip(sw_r.is_intersect).zip(sw_r.box_index).map{case((t, isint), boxidx) => 
              (boxidx, (t, isint))  
            }:_*)

            // traverse the four elements of actual output, verify their
            // ordering is correct, and they match with what the software gold
            // result predicts
            lazy val o = dut.out.bits.peek()
            def error_string = {
              s"input #${input_no}\nactual tmin: ${o.tmin_out.map(bitsToFloat(_))}\n" + s"actual intersect: ${o.isIntersect.map(_.litValue)}\n" +  s"actual boxidx: ${o.boxIndex.map(_.litValue)}\n" + s"Predicted: ${sw_r}"
            }
            var has_seen_non_intersect = false
            var so_far_largest_tmin = 0.0f
            for(idx <- 0 until 4){
              val actual_tmin = bitsToFloat(dut.out.bits.tmin_out(idx).peek())
              val actual_is_intersect = dut.out.bits.isIntersect(idx).peek().litToBoolean
              val actual_box_idx = dut.out.bits.boxIndex(idx).peek().litValue.intValue

              // check t value monotonicity (use scala.Predef.assert instead of
              // scalatest.assert so we can call error_string by-name)
              Predef.assert(actual_tmin >= so_far_largest_tmin, error_string)

              // check intersects go before non-intersects
              Predef.assert(!has_seen_non_intersect || (has_seen_non_intersect && !actual_is_intersect), error_string)

              // check HW and SW yields the same intersect t value for each box
              Predef.assert(input_box_status(actual_box_idx)._1 == actual_tmin, error_string)

              // checkout HW and SW yields the same opinion on intersection
              Predef.assert(input_box_status(actual_box_idx)._2 == actual_is_intersect, error_string)

              if(!has_seen_non_intersect && !actual_is_intersect){
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

        println(s"test ends at time ${dut.exposed_time.peek().litValue}")
      }
    }
  }

  // Define test cases
  testRayBoxIntersection(
    "Small Ray inside box",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(0.5f, -0.5f, 0.5f), float_3(0.0001f, -0.0001f, -0.001f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on edge of box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(1.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing away",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(1.0f, 1.0f, 1.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray on corner of box pointing along edge",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(1.0f, 1.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray outside box pointing towards box",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(1.0f, 0.0f, 0.0f)) :: Nil
  )
  testRayBoxIntersection(
    "Ray hits node 1 then node 2",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
      SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f, 0.5f) :: SW_Box():: SW_Box()::Nil)::Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray hits node 4 then 1 then 2 then misses 3",
    (SW_Box(2.0f, 2.5f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      SW_Box(100.0f, 100.25f, -0.5f, 0.5f, -0.5f, 0.5f) ::
      SW_Box(-5.0f, -4.0f, -0.5f, 0.5f, -0.5f, 0.5f) ::
        SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) ::
       Nil)::Nil,
    new SW_Ray(float_3(-2.0f, 0.0f, 0.0f), float_3(10.0f, 0.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    "Ray outside box pointing along edge",
    (SW_Box(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f) :: SW_Box():: SW_Box():: SW_Box() :: Nil)::Nil,
    new SW_Ray(float_3(1.0f, 2.0f, 1.0f), float_3(0.0f, -1.0f, 0.0f)) :: Nil
  )

  testRayBoxIntersection(
    s"${N_RANDOM_TEST} randomized rays and boxes within range -10000.0, 10000.0",
    List.fill(N_RANDOM_TEST){List.fill(4){RaytracerGold.genRandomBox(1e16.toFloat)}},
    List.fill(N_RANDOM_TEST){RaytracerGold.genRandomRay(1e5.toFloat)}
  )
}
