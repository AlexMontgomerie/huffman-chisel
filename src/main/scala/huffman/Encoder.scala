package huffman

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.util.experimental.loadMemoryFromFileInline

import huffman.Stream

object EncoderState extends ChiselEnum {
  val FILL, DUMP = Value
}

class EncoderIO[T <: UInt](gen: T) extends Bundle {
  val in  = Flipped(Stream(gen))
  val out = Stream(gen)
}

class Encoder[T <: UInt](gen: T, size: Int, code_width: Int, len_width: Int,
  code_file: String, len_file: String) extends Module {

  // get data widths
  val data_width = gen.getWidth

  // create an instance of memory for both codes and their length
  val code_mem = SyncReadMem(size, UInt(code_width.W))
  val len_mem  = SyncReadMem(size, UInt(len_width.W))

  // create a buffer and pointer
  val code_buffer   = RegInit(0.U((2*code_width).W))
  val code_pointer  = RegInit(0.U((8).W))


  // load the buffers from a file
  if (code_file.trim().nonEmpty) {
    loadMemoryFromFileInline(code_mem, code_file)
  }

  if (len_file.trim().nonEmpty) {
    loadMemoryFromFileInline(len_mem, len_file)
  }

  // create the state register
  val state = RegInit(EncoderState.EMPTY)

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := false.B

  // get the code word and length
  val curr_code = Wire(code_mem.read(io.in.bits, io.in.valid))
  val curr_len  = Wire(len_mem.read(io.in.bits, io.in.valid))

  // last signal buffer
  val last_buffer := RegInit(false.B)

  // state machine logic
  switch (state) {
    is(EncoderState.Fill) {
      when (io.in.valid) {
        // append the current code word to the code word buffer,
        // and update the pointer
        code_buffer   := code_buffer | ( curr_code << code_pointer )
        code_pointer  := code_pointer + curr_len
      }
      when (code_pointer >= code_width || io.in.last) {
        // move to the DUMP state if the code_pointer is overflowing
        state := EncoderState.DUMP
      }
      // set the ready signal
      io.in.ready := true.B
      // cache the last signal
      last_buffer := io.in.last
      // assign no output
      io.out.bits   := DontCare
      io.out.valid  := false.B
      io.out.last   := false.B
    }
    is(EncoderState.DUMP) {
      when (io.out.ready) {
        // reduce the code buffer and pointer
        code_buffer   := code_buffer >> code_width
        code_pointer  := code_pointer - code_width
        // go back to the FILL state
        state := EncoderState.FILL
      }
      // set the output
      io.out.bits   := code_buffer
      io.out.valid  := true.B
      io.out.last   := last_buffer
      // set the input to not ready
      io.in.ready := false.B
    }
  }
}
