package baseline_datapath

import chisel3._
import chisel3.util._

object SkidBufferState extends ChiselEnum{
  /// Both skid and output buffer are empty
  val Empty = Value

  /// Output buffer has something valid for the consumer, skid buffer empty
  val Busy = Value

  /// Both skid and output buffers are full
  val Full = Value
}

class SkidBufferStage[T <: Data](
  private val gen: T,
  transfer_function: T=>T = (x: T) => identity(x)
) extends Module{
  val intake = IO(Flipped(Decoupled(gen)))
  val emit = IO(Decoupled(gen))

  // internally, the input is called "in", the output is called "out"
  val in = Wire(Flipped(Decoupled(gen)))
  in :<>= intake

  val out = Wire(Decoupled(gen))
  emit :<>= out

  val state = RegInit(SkidBufferState.Empty)

  val skid_buffer = Reg(gen)
  val output_buffer = Reg(gen)

  // interface handshake signals
  out.bits := output_buffer
  out.valid := (state =/= SkidBufferState.Empty)
  in.ready := (state =/= SkidBufferState.Full)

  // internal control signals
  val load = WireInit(state === SkidBufferState.Empty && in.fire)
  val unload = WireInit(state === SkidBufferState.Busy && out.fire && !in.fire)
  val flow = WireInit(state === SkidBufferState.Busy && in.fire && out.fire)
  val fill = WireInit(state === SkidBufferState.Busy && in.fire && !out.fire)
  val flush = WireInit(state === SkidBufferState.Full && out.fire)

  // state transition
  switch(state){
    is(SkidBufferState.Empty){
      when(in.fire){
        state := SkidBufferState.Busy
      }.otherwise{
        // maintain
      }
    }
    is(SkidBufferState.Busy){
      when(in.fire && out.fire){
        // maintain
      }.elsewhen(in.fire && !out.fire){
        state := SkidBufferState.Full
      }.elsewhen(!in.fire && out.fire){
        state := SkidBufferState.Empty
      }.otherwise{
        // maintain
      }
    }
    is(SkidBufferState.Full){
      when(out.fire){
        state := SkidBufferState.Busy
      }.otherwise{
        // maintain
      }
    }
  }
  
  // output buffer data
  val transfer_function_input = Mux(state===SkidBufferState.Full, skid_buffer, in.bits)
  when(load || flow || flush){
    output_buffer := transfer_function(transfer_function_input)
  }

  // skid buffer data
  when(fill){
    skid_buffer := in.bits
  }

}

// class SkidBuffer[T <: Data](private val gen: T) extends Module{
//   val in = IO(Flipped(Decoupled(gen)))
//   val out = IO(Decoupled(gen))

//   val state = RegInit(SkidBufferState.Empty)
//   val skid_buffer = Reg(Valid(gen))

//   val (_time, _) = Counter(true.B, (1 << 31) - 1)
//   dontTouch(_time)

//   // next state logic
//   switch(state){
//     is(SkidBufferState.Empty){
//       when(in.valid && out.ready){

//       }.elsewhen(in.valid && !out.ready){
//         state := SkidBufferState.Full
//       }.elsewhen(!in.valid && out.ready){

//       }.elsewhen(!in.valid && !out.ready){

//       }
//     }
//     is(SkidBufferState.Full){
//       when(in.valid && out.ready){

//       }.elsewhen(in.valid && !out.ready){

//       }.elsewhen(!in.valid && out.ready){
//         state := SkidBufferState.Empty
//       }.elsewhen(!in.valid && !out.ready){

//       }
//     }
//   }

//   out.valid := false.B
//   out.bits := in.bits 
//   in.ready := false.B

//   // output signals
//   switch(state){
//     is(SkidBufferState.Empty){
//       out.valid := in.valid
//       out.bits := in.bits
//       when(in.valid && out.ready){
//         in.ready := true.B 
//       }.elsewhen(in.valid && !out.ready){
//         // state := SkidBufferState.Full
//         in.ready := true.B 
//         skid_buffer.bits := in.bits 
//         skid_buffer.valid := in.valid
//       }.elsewhen(!in.valid && out.ready){
//         in.ready := true.B 
//       }.elsewhen(!in.valid && !out.ready){
//         in.ready := true.B 
//       }
//     }
//     is(SkidBufferState.Full){
//       out.valid := skid_buffer.valid 
//       out.bits := skid_buffer.bits 
//       when(in.valid && out.ready){
//         in.ready := true.B
//         skid_buffer.bits := in.bits
//         skid_buffer.valid := in.valid
//       }.elsewhen(in.valid && !out.ready){
//         in.ready := false.B 
//       }.elsewhen(!in.valid && out.ready){
//         // state := SkidBufferState.Empty
//         in.ready := true.B 
//         skid_buffer.bits := in.bits
//         skid_buffer.valid := in.valid
//       }.elsewhen(!in.valid && !out.ready){
//         in.ready := false.B 
//       }
//     }
//   }

//   // printf(cf"SkidBuffer ${_time}: state: ${state.asUInt}, input: ${in}, output: ${out}\n")
// }

// class SkidBufferStage[T <: Data](
//   private val gen: T, 
//   transfer_function: Valid[T]=>T = (x: Valid[T]) => identity(x).bits
// ) extends Module{
//   val intake = IO(Flipped(Decoupled(gen)))
//   val emit = IO(Decoupled(gen))

//   val (_time, _) = Counter(true.B, (1 << 31) - 1)
//   dontTouch(_time)

//   val sb = Module(new SkidBufferToo(gen))

//   sb.in :<>= intake
//   emit :<>= sb.out

//   // overwrite emit.bits
//   val wrapped_bits = Wire(Valid(gen))
//   wrapped_bits.bits := sb.out.bits 
//   wrapped_bits.valid := sb.out.valid
//   emit.bits := transfer_function(wrapped_bits)

//   // printf(cf"SkidBufferStage: intake: ${intake}, emit: ${emit}\n")
//   printf(cf"SkidBufferToo ${name} ${_time}: state: ${sb.exposed_state}, input: ${intake}, output: ${emit}\n")
// }