package baseline_datapath

import chisel3._
import chisel3.util._

object SkidBufferState extends ChiselEnum{
  val Empty, Full = Value
}

class SkidBuffer[T <: Data](private val gen: T) extends Module{
  val in = IO(Flipped(Decoupled(gen)))
  val out = IO(Decoupled(gen))

  val state = RegInit(SkidBufferState.Empty)
  val skid_buffer = Reg(Valid(gen))

  val (_time, _) = Counter(true.B, (1 << 31) - 1)
  dontTouch(_time)

  // next state logic
  switch(state){
    is(SkidBufferState.Empty){
      when(in.valid && out.ready){

      }.elsewhen(in.valid && !out.ready){
        state := SkidBufferState.Full
      }.elsewhen(!in.valid && out.ready){

      }.elsewhen(!in.valid && !out.ready){

      }
    }
    is(SkidBufferState.Full){
      when(in.valid && out.ready){

      }.elsewhen(in.valid && !out.ready){

      }.elsewhen(!in.valid && out.ready){
        state := SkidBufferState.Empty
      }.elsewhen(!in.valid && !out.ready){

      }
    }
  }

  out.valid := false.B
  out.bits := in.bits 
  in.ready := false.B

  // output signals
  switch(state){
    is(SkidBufferState.Empty){
      out.valid := in.valid
      out.bits := in.bits
      when(in.valid && out.ready){
        in.ready := true.B 
      }.elsewhen(in.valid && !out.ready){
        // state := SkidBufferState.Full
        in.ready := false.B 
        skid_buffer.bits := in.bits 
        skid_buffer.valid := in.valid
      }.elsewhen(!in.valid && out.ready){
        in.ready := true.B 
      }.elsewhen(!in.valid && !out.ready){
        in.ready := true.B 
      }
    }
    is(SkidBufferState.Full){
      out.valid := skid_buffer.valid 
      out.bits := skid_buffer.bits 
      when(in.valid && out.ready){
        in.ready := true.B
        skid_buffer.bits := in.bits
        skid_buffer.valid := in.valid
      }.elsewhen(in.valid && !out.ready){
        in.ready := false.B 
      }.elsewhen(!in.valid && out.ready){
        // state := SkidBufferState.Empty
        in.ready := true.B 
      }.elsewhen(!in.valid && !out.ready){
        in.ready := false.B 
      }
    }
  }

  printf(cf"SkidBuffer ${_time}: state: ${state.asUInt}, input: ${in}, output: ${out}\n")
}

class SkidBufferStage[T <: Data](
  private val gen: T, 
  transfer_function: Valid[T]=>T = (x: Valid[T]) => identity(x).bits
) extends Module{
  val intake = IO(Flipped(Decoupled(gen)))
  val emit = IO(Decoupled(gen))

  val sb = Module(new SkidBuffer[T](gen))
  sb.in:<>= intake
  emit :<>= sb.out

  // overwrite emit.bits
  val wrapped_bits = Wire(Valid(gen))
  wrapped_bits.bits := sb.out.bits 
  wrapped_bits.valid := sb.out.valid
  emit.bits := transfer_function(wrapped_bits)

  printf(cf"SkidBufferStage: intake: ${intake}, emit: ${emit}\n")
}