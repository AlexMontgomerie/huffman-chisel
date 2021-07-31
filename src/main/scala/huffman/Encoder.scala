package huffman

import math._
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType
import firrtl.FileUtils

import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.MemorySynthInit

class EncoderIO[T <: UInt](gen: T) extends Bundle {
  val in  = Flipped(Stream(gen))
  val out = Stream(gen)
}

class Encoder[T <: UInt](gen: T, code_file: String, len_file: String) extends Module {

  // update annotations to allow FPGA synthesis
  annotate(new ChiselAnnotation {
    override def toFirrtl =
      MemorySynthInit
  })

  // parse the code and len tables
  val codes = FileUtils.getLines(code_file).map(Integer.parseInt(_,16))
  val lens  = FileUtils.getLines(len_file).map(Integer.parseInt(_,16))

  // get the min and max lengths
  val min_len = lens.map(_.toInt).min
  val max_len = lens.map(_.toInt).max

  // get data widths
  val data_width = gen.getWidth
  val code_width = lens.map(_.toInt).max
  val len_width  = ( log(code_width) / log(2.0) ).ceil.toInt
  val ptr_width  = ( log(2*code_width) / log(2.0) ).ceil.toInt

  // initialise IO
  val io = IO(new EncoderIO(gen))

  // create an instance of memory for both codes and their length
  val code_mem = SyncReadMem(pow(2,data_width).toInt, UInt(code_width.W))
  val len_mem  = SyncReadMem(pow(2,data_width).toInt, UInt(len_width.W))

  // create a buffer and pointer
  val code_buffer_data  = RegInit(0.U((2*code_width).W))
  val code_buffer_ptr   = RegInit(0.U((ptr_width).W))

  // load the buffers from a file
  if (code_file.trim().nonEmpty) {
    loadMemoryFromFileInline(code_mem, code_file, hexOrBinary=MemoryLoadFileType.Hex)
  }
  if (len_file.trim().nonEmpty) {
    loadMemoryFromFileInline(len_mem, len_file, hexOrBinary=MemoryLoadFileType.Hex)
  }

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := false.B

  // registers for the inputs
  val input_bits  = RegNext(io.in.bits)
  val input_valid = RegNext(io.in.valid)
  val input_last  = io.in.last

  // get the code word and length
  val curr_code = code_mem.read(io.in.bits, io.in.valid)
  val curr_len  = len_mem.read(io.in.bits, io.in.valid)

  // valid signal for the output
  val output_valid = RegInit(false.B)

  // a latch to detect the last output
  val last_buffer = RegInit(false.B)
  when(io.in.last) {
    last_buffer := true.B
  }

  // create a state machine for the buffer
  val buffer_state = RegInit(BufferState.EMPTY)

  switch(buffer_state) {
    is(BufferState.EMPTY) {
      when(input_valid && io.out.ready) {
        val next_ptr = code_buffer_ptr + curr_len
        code_buffer_data := ( code_buffer_data << curr_len ) | curr_code
        code_buffer_ptr   := next_ptr
        when(last_buffer) {
          buffer_state := BufferState.DONE
          output_valid  := true.B
          io.in.ready   := false.B
        } .elsewhen(next_ptr >= data_width.U) {
          buffer_state := BufferState.FILL
          output_valid  := true.B
          io.in.ready   := true.B
        } .otherwise {
          buffer_state := BufferState.EMPTY
          output_valid  := false.B
          io.in.ready   := true.B
        }
      } .otherwise {
        // keep the code buffer and pointer the same
        code_buffer_data := code_buffer_data
        code_buffer_ptr  := code_buffer_ptr
        // update state
        buffer_state := BufferState.EMPTY
        // update valid signal
        output_valid := false.B
        // set the ready signal
        io.in.ready := true.B
      }
    }
    is(BufferState.FILL) {
      when(input_valid && io.out.ready) {
        //
        val next_ptr = code_buffer_ptr + curr_len - data_width.U
        code_buffer_data := ( code_buffer_data << curr_len ) | curr_code
        code_buffer_ptr   := next_ptr
        val next_buffer_state = WireDefault(BufferState.FILL)
        when(last_buffer) {
          buffer_state := BufferState.DONE
          output_valid  := true.B
          io.in.ready   := false.B
        } .elsewhen(next_ptr >= (2*data_width).U) {
          buffer_state := BufferState.FULL
          output_valid  := true.B
          io.in.ready   := false.B
        } .elsewhen(next_ptr >= data_width.U) {
          buffer_state := BufferState.FILL
          output_valid  := true.B
          io.in.ready   := true.B
        } .otherwise {
          buffer_state := BufferState.EMPTY
          output_valid  := false.B
          io.in.ready   := true.B
        }
      } .otherwise {
        // keep the code buffer and pointer the same
        code_buffer_data := code_buffer_data
        code_buffer_ptr  := code_buffer_ptr
        // update state
        buffer_state := BufferState.FILL
        // update valid signal
        output_valid := false.B
        // set the ready signal
        io.in.ready := true.B
      }
    }
    is(BufferState.FULL) {
      when(io.out.ready) {
        // update the code buffer pointer
        code_buffer_data := code_buffer_data
        val next_ptr = code_buffer_ptr - data_width.U
        code_buffer_ptr   := next_ptr
        when(last_buffer){
          buffer_state := BufferState.DONE
          output_valid  := true.B
          io.in.ready   := false.B
        } .elsewhen(next_ptr >= (2*data_width).U) {
          buffer_state := BufferState.FULL
          output_valid  := true.B
          io.in.ready   := false.B
        } .elsewhen(next_ptr >= data_width.U) {
          buffer_state := BufferState.FILL
          output_valid  := true.B
          io.in.ready   := true.B
        } .otherwise {
          buffer_state := BufferState.EMPTY
          output_valid  := false.B
          io.in.ready   := true.B
        }
      } .otherwise {
        // keep the code buffer and pointer the same
        code_buffer_data := code_buffer_data
        code_buffer_ptr  := code_buffer_ptr
        // update state
        buffer_state := BufferState.FULL
        // update valid signal
        output_valid := false.B
        // set the ready signal
        io.in.ready := true.B
      }
    }
    is(BufferState.DONE) {
      when(io.out.ready) {
        code_buffer_data := code_buffer_data
        code_buffer_ptr := code_buffer_ptr - data_width.U
        buffer_state := BufferState.DONE
        output_valid  := true.B
        io.in.ready   := false.B
      } .otherwise {
        // keep the code buffer and pointer the same
        code_buffer_data := code_buffer_data
        code_buffer_ptr  := code_buffer_ptr
        // update state
        buffer_state := BufferState.DONE
        // update valid signal
        output_valid := false.B
        // set the ready signal
        io.in.ready := true.B
        io.out.bits := code_buffer_data >> (code_buffer_ptr - data_width.U)
      }
    }
  }

  // connect lower code_buffer bits to the output
  io.out.bits := (code_buffer_data << data_width.U) >> code_buffer_ptr
  io.out.valid  := output_valid

  // last signal logic
  when(buffer_state === BufferState.DONE && last_buffer && (code_buffer_ptr.asSInt <= data_width.S) ) {
    io.out.last := true.B
    last_buffer := false.B
  } .otherwise {
    io.out.last := false.B
  }

}

class BufferedEncoder[T <: UInt](gen: T, code_file: String, len_file: String) extends Module {

  // initialise IO
  val io = IO(new EncoderIO(gen))

  // initialise encoder
  val encoder = Module( new Encoder[T](gen, code_file, len_file) )

  // get the data width
  val data_width = gen.getWidth

  // create a queue for incoming samples
  val queue = Module(new Queue(Bits((data_width+1).W), 2)).io

  // connect the input of the queue
  queue.enq.bits := Cat(io.in.last, io.in.bits)
  queue.enq.valid := io.in.valid
  io.in.ready := queue.enq.ready

  // connect the output of the queue to the encoder
  encoder.io.in.bits  := queue.deq.bits
  encoder.io.in.valid := queue.deq.valid
  encoder.io.in.last  := queue.deq.bits >> data_width
  queue.deq.ready := encoder.io.in.ready

  // connect output directly
  io.out.bits   := encoder.io.out.bits
  io.out.valid  := encoder.io.out.valid
  io.out.last   := encoder.io.out.last
  encoder.io.out.ready := io.out.ready

}
