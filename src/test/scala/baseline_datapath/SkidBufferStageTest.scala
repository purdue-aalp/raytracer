package baseline_datapath

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

import chiseltest.internal.CachingAnnotation
import firrtl2.options.TargetDirAnnotation
import chisel3.stage.{
  PrintFullStackTraceAnnotation,
  ThrowOnFirstErrorAnnotation
}

class ChainedSkidBufferStages extends Module{
  val intake = IO(Flipped(Decoupled(UInt(10.W))))
  val emit = IO(Decoupled(UInt(10.W)))

  val stage1 = Module(new SkidBufferStage(UInt(10.W), {(x:UInt)=>x + 9.U})).suggestName("stage1")
  val stage2 = Module(new SkidBufferStage(UInt(10.W), {(x:UInt)=>x * 11.U})).suggestName("stage2")
  val stage3 = Module(new SkidBufferStage(UInt(10.W), {(x:UInt)=>x - 9.U})).suggestName("stage3")

  stage1.intake :<>= intake 
  stage2.intake :<>= stage1.emit 
  stage3.intake :<>= stage2.emit
  emit :<>= stage3.emit
}

class SkidBufferStageTest extends AnyFreeSpec with ChiselScalatestTester {
  val r = new Random(420)
  val input_prob = 0.5f
  val output_prob = 0.5f
  val SEQ_LENGTH = 1000

  val chisel_test_annotations = Seq(
    // VerilatorBackendAnnotation,
    CachingAnnotation,
    // CachingDebugAnnotation,
    // TargetDirAnnotation(
    //   if (use_stage_submodule)
    //     "cached_verilator_backend/Datapath_stage_submodule"
    //   else "cached_verilator_backend/Datapath_monolithic"
    // ),
    // WriteVcdAnnotation,
    // VerilatorCFlags(Seq("-O3", "-march=native")),
    // VerilatorLinkFlags(Seq("-O3", "-march=native")),
    // VerilatorFlags(Seq("-O3", "--threads", "8"))
  )
  val chisel_test_chisel_annotations = Seq(
    ThrowOnFirstErrorAnnotation,
    PrintFullStackTraceAnnotation
  )

  val val_seq: List[Int] = List.from(0 until SEQ_LENGTH)

  // "skidbuffer stage should pass through identical values from 0 to 1000 when both ends are free" in {
  //   test(new SkidBufferStage(UInt(10.W)))
  //     .withAnnotations(chisel_test_annotations)
  //     .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
  //       dut.intake.initSource().setSourceClock(dut.clock)
  //       dut.emit.initSink().setSinkClock(dut.clock)

  //       fork{
  //         dut.intake.enqueueSeq(val_seq.map(_.U(10.W)))
  //       }.fork{
  //         dut.emit.expectDequeueSeq(val_seq.map(_.U(10.W)))    
  //       }.join()
  //     }
  // }

  // "skidbuffer stage should pass through identical values from 0 to 1000 when emit is congested" in {
  //   test(new SkidBufferStage(UInt(10.W)))
  //     .withAnnotations(chisel_test_annotations)
  //     .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
  //       dut.intake.initSource().setSourceClock(dut.clock)
  //       dut.emit.initSink().setSinkClock(dut.clock)

  //       fork{
  //         dut.intake.enqueueSeq(val_seq.map(_.U(10.W)))
  //       }.fork{
  //         val_seq.foreach{idx => 
  //           dut.emit.ready.poke(false.B)
  //           while(r.nextFloat()<=output_prob){
  //             dut.clock.step()
  //           } 
            
  //           // println(s"output expects ${idx}")
  //           dut.emit.expectDequeue(idx.U(10.W))
  //         }  
  //       }.join()
  //     }
  // }

  // "skidbuffer stage should pass through identical values from 0 to 1000 when intake is throttled" in {
  //   test(new SkidBufferStage(UInt(10.W)))
  //     .withAnnotations(chisel_test_annotations)
  //     .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
  //       dut.intake.initSource().setSourceClock(dut.clock)
  //       dut.emit.initSink().setSinkClock(dut.clock)

  //       fork{
  //         val_seq.foreach{idx => 
  //           dut.intake.valid.poke(false.B)
  //           while(r.nextFloat() <= input_prob){
  //             dut.clock.step()
  //           }  
  //           // println(s"intake pokes ${idx}")
  //           dut.intake.enqueue(idx.U(10.W))
  //         }
  //       }.fork{
  //         dut.emit.expectDequeueSeq(val_seq.map(_.U(10.W))) 
  //       }.join()
  //     }
  // }

  // "skidbuffer stage should pass through identical values from 0 to 1000 when intake is throttled and emit is congested" in {
  //   test(new SkidBufferStage(UInt(10.W)))
  //     .withAnnotations(chisel_test_annotations)
  //     .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
  //       dut.intake.initSource().setSourceClock(dut.clock)
  //       dut.emit.initSink().setSinkClock(dut.clock)

  //       fork{
  //         val_seq.foreach{idx => 
  //           dut.intake.valid.poke(false.B)
  //           while(r.nextFloat() <= input_prob){
  //             dut.clock.step()
  //           }  
  //           // println(s"intake pokes ${idx}")
  //           dut.intake.enqueue(idx.U(10.W))
  //         }
  //       }.fork{
  //         val_seq.foreach{idx => 
  //           dut.emit.ready.poke(false.B)
  //           while(r.nextFloat()<=output_prob){
  //             dut.clock.step()
  //           } 
  //           // println(s"output expects ${idx}")
  //           dut.emit.expectDequeue(idx.U(10.W))
  //         }  
  //       }.join()
  //     }
  // }

  // "skidbuffer stage should pass through squared values from 0 to 1000 when intake is throttled and emit is congested" in {
  //   def square_uint(x: Valid[UInt]): UInt = {
  //     x.bits * x.bits
  //   }
  //   test(new SkidBufferStage(UInt(10.W), square_uint))
  //     .withAnnotations(chisel_test_annotations)
  //     .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
  //       dut.intake.initSource().setSourceClock(dut.clock)
  //       dut.emit.initSink().setSinkClock(dut.clock)

  //       fork{
  //         val_seq.take(30).foreach{idx => 
  //           dut.intake.valid.poke(false.B)
  //           while(r.nextFloat() <= input_prob){
  //             dut.clock.step()
  //           }  
  //           // println(s"intake pokes ${idx}")
  //           dut.intake.enqueue(idx.U(10.W))
  //         }
  //       }.fork{
  //         val_seq.take(30).foreach{idx => 
  //           dut.emit.ready.poke(false.B)
  //           while(r.nextFloat()<=output_prob){
  //             dut.clock.step()
  //           } 
  //           // println(s"output expects ${idx}")
  //           dut.emit.expectDequeue((idx*idx).U(10.W))
  //         }  
  //       }.join()
  //     }
  // }

  "chained skidbuffer stages should pass through correct values from 0 to 1000 when intake is throttled and emit is congested" in {
    def square_uint(x: Valid[UInt]): UInt = {
      x.bits * x.bits
    }
    test(new ChainedSkidBufferStages())
      .withAnnotations(chisel_test_annotations)
      .withChiselAnnotations(chisel_test_chisel_annotations) { dut =>
        dut.intake.initSource().setSourceClock(dut.clock)
        dut.emit.initSink().setSinkClock(dut.clock)

        fork{
          val_seq.take(30).foreach{idx => 
            dut.intake.valid.poke(false.B)
            while(r.nextFloat() <= input_prob){
              dut.clock.step()
            }  
            // println(s"intake pokes ${idx}")
            dut.intake.enqueue(idx.U(10.W))
          }
        }.fork{
          val_seq.take(30).foreach{idx => 
            dut.emit.ready.poke(false.B)
            while(r.nextFloat()<=output_prob){
              dut.clock.step()
            } 
            // println(s"output expects ${idx}")
            dut.emit.expectDequeue((idx*11+90).U(10.W))
          }  
        }.join()
      }
  }
}