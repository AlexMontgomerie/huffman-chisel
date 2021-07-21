package huffman

import math._
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.FileUtils

object DecoderState extends ChiselEnum {
  val FILL, FULL, DUMP = Value
}

object BufferState extends ChiselEnum {
  val EMPTY, FILL, FULL, DONE = Value
}

class DecoderIO[T <: UInt](gen: T) extends Bundle {
  val in  = Flipped(Stream(gen))
  val out = Stream(gen)
}

class Decoder[T <: UInt](gen: T, size: Int, code_width: Int, len_width: Int, code_file: String, len_file: String) extends Module {

  // get data widths
  val data_width = gen.getWidth
  val ptr_width  = ( log(2*code_width) / log(2.0) ).ceil.toInt

  // initialise IO
  val io = IO(new DecoderIO(gen))

  // create the code table from scala
  val codes = FileUtils.getLines(code_file)
  val lens  = FileUtils.getLines(len_file)
  val codes_lens = (codes zip lens).zipWithIndex.map(x => (x._1._1.toInt, x._1._2.toInt, x._2.toInt)).sortWith((a,b) => a._2 < a._2)

  // registers for data and length
  val curr_data = RegInit(0.U(data_width.W))
  val curr_len  = RegInit(0.U(len_width.W))

  // store the previous data and length also
  val prev_data = RegNext(curr_data)
  val prev_len  = RegNext(curr_len)

  // create the states for rle
  val state = RegInit(DecoderState.FILL)

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := true.B

  // create a buffer and pointer
  val code_buffer   = RegInit(0.U((3*code_width).W))
  val code_pointer  = RegInit(0.U((ptr_width).W))

  val next_code_buffer  = Wire(UInt((3*code_width).W))
  val next_code_pointer = Wire(UInt((ptr_width).W))

  // registered input signals
  val input_bits  = io.in.bits
  val input_valid = io.in.valid

  // create a state machine for the buffer
  val buffer_state = RegInit(BufferState.FILL)

  // defaults for next signals
  next_code_buffer   := code_buffer
  next_code_pointer  := code_pointer

  // a latch to detect the last output
  val last_buffer = RegInit(false.B)
  when(io.in.last) {
    last_buffer := true.B
  }

  // last signal logic
  when(last_buffer && (next_code_pointer === 0.U) ) {
    io.out.last := true.B
  } .otherwise {
    io.out.last := false.B
  }

  // valid signal for the output
  val output_valid = RegInit(false.B)

  switch(buffer_state) {
    is(BufferState.FILL) {
      when(input_valid && io.out.ready) {
        // only update the code buffer if the output is ready and the input is valid
        next_code_buffer   := ( code_buffer | ( input_bits << code_pointer ) ) >> curr_len
        next_code_pointer  := code_pointer + data_width.U - curr_len
        // set the output signal to valid
        output_valid := true.B
        when(next_code_pointer >= (2*code_width).U || io.in.last) {
          // if the code buffer is about to overflow, just output instead
          buffer_state := BufferState.FULL
          io.in.ready := false.B
        } .otherwise {
          // otherwise, stay in the current state
          buffer_state := BufferState.FILL
          io.in.ready := true.B
        }
      }.otherwise {
        // keep the code buffer and pointer the same
        next_code_buffer   := code_buffer
        next_code_pointer  := code_pointer
        buffer_state := BufferState.FILL
        io.in.ready := true.B
        output_valid := false.B
      }
    }
    is(BufferState.FULL) {
      when (io.out.ready) {
        // shift out the code words in the buffer
        next_code_buffer   := code_buffer >> curr_len
        next_code_pointer  := code_pointer - curr_len
        output_valid := true.B
        when(next_code_pointer >= (2*code_width).U || last_buffer) {
          buffer_state := BufferState.FULL
          io.in.ready := false.B
        } .otherwise {
          buffer_state := BufferState.FILL
          io.in.ready := true.B
        }
      } .otherwise {
        // keep the buffer the same
        next_code_buffer   := code_buffer
        next_code_pointer  := code_pointer
        buffer_state := BufferState.FULL
        io.in.ready := false.B
        output_valid := false.B
      }
    }
  }

  // update the code buffer and pointer
  code_buffer   := next_code_buffer
  code_pointer  := next_code_pointer

  // define a method to update the current lengths
  def update_curr_data_and_len(data: UInt, len: UInt) {
    curr_data := data
    curr_len  := len
  }

  // bit mask for correctly comparing code buffer
  val mask = (x: Int) => ((1<<x)-1).U

  // map codes and lens to a list of conditions and actions
  val cond_act_pairs = codes_lens.map( x => (
    ((next_code_buffer & mask(x._2)) === x._1.U(x._2.W)) && (next_code_pointer >= x._2.U),
    { () => update_curr_data_and_len(x._3.U, x._2.U) }
  ))

  // create the when context
  var context = {
    val (cond, action) = cond_act_pairs.head
    when(cond)(action())
  }
  for((cond, action) <- cond_act_pairs.tail) {
    context = context.elsewhen(cond)(action())
  }
  context.otherwise {
    curr_data := 0.U
    curr_len  := 0.U
  }

  // assign the output bits to the decoded word
  io.out.bits   := curr_data
  io.out.valid  := output_valid
}

class BufferedDecoder[T <: UInt](gen: T, size: Int, code_width: Int, len_width: Int, code_file: String, len_file: String) extends Module {

  // initialise IO
  val io = IO(new DecoderIO(gen))

  // initialise decoder
  val decoder = Module( new Decoder[T](gen, size, code_width, len_width, code_file, len_file) )

  // connect inputs with registers
  decoder.io.in.bits  := RegNext(io.in.bits)
  decoder.io.in.valid := RegNext(io.in.valid)
  decoder.io.in.last  := RegNext(io.in.last)
  // io.in.ready := RegNext(decoder.io.in.ready)
  io.in.ready := decoder.io.in.ready

  // connect output directly
  io.out.bits   := decoder.io.out.bits
  io.out.valid  := decoder.io.out.valid
  io.out.last   := decoder.io.out.last
  decoder.io.out.ready := io.out.ready

}
