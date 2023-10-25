package baseline_datapath

import chisel3._
import chisel3.util._
import chisel3.util.experimental._
object SkidBufferState extends ChiselEnum {
  /// Both skid and output buffer are empty
  val Empty = Value

  /// Output buffer has something valid for the consumer, skid buffer empty
  val Busy = Value

  /// Both skid and output buffers are full
  val Full = Value
}

class GenerializedSkidBufferController[T <: Data, V <: Data](
    private val intake_gen: T,
    private val emit_gen: V
) extends Module {
  val intake = IO(Flipped(Decoupled(intake_gen)))
  val emit = IO(Decoupled(emit_gen))
  val send_to_logic = IO(Output(intake_gen))
  val get_from_logic = IO(Input(emit_gen))

  // internally, the input is called "in", the output is called "out"
  val in = Wire(Flipped(Decoupled(intake_gen)))
  in :<>= intake

  val out = Wire(Decoupled(emit_gen))
  emit :<>= out

  val state = RegInit(SkidBufferState.Empty)

  val skid_buffer = Reg(intake_gen)
  val output_buffer = Reg(emit_gen)

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
  switch(state) {
    is(SkidBufferState.Empty) {
      when(in.fire) {
        state := SkidBufferState.Busy
      }.otherwise {
        // maintain
      }
    }
    is(SkidBufferState.Busy) {
      when(in.fire && out.fire) {
        // maintain
      }.elsewhen(in.fire && !out.fire) {
        state := SkidBufferState.Full
      }.elsewhen(!in.fire && out.fire) {
        state := SkidBufferState.Empty
      }.otherwise {
        // maintain
      }
    }
    is(SkidBufferState.Full) {
      when(out.fire) {
        state := SkidBufferState.Busy
      }.otherwise {
        // maintain
      }
    }
  }

  // output buffer data
  val transfer_function_input =
    Mux(state === SkidBufferState.Full, skid_buffer, in.bits)
  send_to_logic := transfer_function_input
  when(load || flow || flush) {
    output_buffer := get_from_logic
  }

  // skid buffer data
  when(fill) {
    skid_buffer := in.bits
  }
}

// The apply() methods of GenerializedSkidBufferStage and SkidBufferStage will
// return an object with this trait. The trait guarantees the module has the
// intake-emit interface.
trait SkidBufferStageModule[T <: Data, V <: Data] extends Module {
  val intake: DecoupledIO[T]
  val emit: DecoupledIO[V]
}

object GenerializedSkidBufferStage {
  def apply[T <: Data, V <: Data](
      intake_gen: T,
      emit_gen: V,
      logic: T => V
  ): SkidBufferStageModule[T, V] = new Module with SkidBufferStageModule[T, V] {
    val intake = IO(Flipped(Decoupled(intake_gen)))
    val emit = IO(Decoupled(emit_gen))

    val ctrl = Module(
      new GenerializedSkidBufferController(intake_gen, emit_gen)
    )
    ctrl.intake :<>= intake
    emit :<>= ctrl.emit
    ctrl.get_from_logic := logic(ctrl.send_to_logic)
  }
}

object SkidBufferStage {
  def apply[T <: Data](
      gen: T,
      logic: T => T = { (x: T) => identity(x) }
  ): SkidBufferStageModule[T, T] = {
    GenerializedSkidBufferStage[T, T](gen, gen, logic)
  }
}

// class SkidBufferStage[T <: Data](
//     private val gen: T,
//     transfer_function: T => T = (x: T) => identity(x)
// ) extends Module {
//   val intake = IO(Flipped(Decoupled(gen)))
//   val emit = IO(Decoupled(gen))

//   val wrapped = Module(
//     new GenerializedSkidBufferStage(gen, gen, transfer_function)
//   )
//   wrapped.intake :<>= intake
//   emit :<>= wrapped.emit
// }

// class SkidBufferStage[T <: Data](
//   private val gen: T,
//   transfer_function: T=>T = (x: T) => identity(x)
// ) extends Module{
//   val intake = IO(Flipped(Decoupled(gen)))
//   val emit = IO(Decoupled(gen))

//   // internally, the input is called "in", the output is called "out"
//   val in = Wire(Flipped(Decoupled(gen)))
//   in :<>= intake

//   val out = Wire(Decoupled(gen))
//   emit :<>= out

//   val state = RegInit(SkidBufferState.Empty)

//   val skid_buffer = Reg(gen)
//   val output_buffer = Reg(gen)

//   // interface handshake signals
//   out.bits := output_buffer
//   out.valid := (state =/= SkidBufferState.Empty)
//   in.ready := (state =/= SkidBufferState.Full)

//   // internal control signals
//   val load = WireInit(state === SkidBufferState.Empty && in.fire)
//   val unload = WireInit(state === SkidBufferState.Busy && out.fire && !in.fire)
//   val flow = WireInit(state === SkidBufferState.Busy && in.fire && out.fire)
//   val fill = WireInit(state === SkidBufferState.Busy && in.fire && !out.fire)
//   val flush = WireInit(state === SkidBufferState.Full && out.fire)

//   // state transition
//   switch(state){
//     is(SkidBufferState.Empty){
//       when(in.fire){
//         state := SkidBufferState.Busy
//       }.otherwise{
//         // maintain
//       }
//     }
//     is(SkidBufferState.Busy){
//       when(in.fire && out.fire){
//         // maintain
//       }.elsewhen(in.fire && !out.fire){
//         state := SkidBufferState.Full
//       }.elsewhen(!in.fire && out.fire){
//         state := SkidBufferState.Empty
//       }.otherwise{
//         // maintain
//       }
//     }
//     is(SkidBufferState.Full){
//       when(out.fire){
//         state := SkidBufferState.Busy
//       }.otherwise{
//         // maintain
//       }
//     }
//   }

//   // output buffer data
//   val transfer_function_input = Mux(state===SkidBufferState.Full, skid_buffer, in.bits)
//   when(load || flow || flush){
//     output_buffer := transfer_function(transfer_function_input)
//   }

//   // skid buffer data
//   when(fill){
//     skid_buffer := in.bits
//   }

// }
