package huffman

import scala.io.Source

import math._
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import firrtl.FileUtils

object BufferState extends ChiselEnum {
  val EMPTY, FILL, FULL, DONE = Value
}

class DecoderIO[T <: UInt](gen: T) extends Bundle {
  val in  = Flipped(Stream(gen))
  val out = Stream(gen)
}

class LookUpTable[T <: UInt](gen: T, code_data: Seq[(Int,Int)]) extends Module {

  // define the IO
  val io = IO(new Bundle {
    val in    = Input(gen)
    val out   = Output(UInt(8.W))
    val valid = Output(Bool())
  })

  // generate the LUT
  var context = {
    val (code, data) = code_data.head
    when(io.in === code.U) {
      io.out := data.U
      io.valid := true.B
    }
  }
  for((code, data) <- code_data.tail) {
    context = context.elsewhen(io.in === code.U) {
      io.out := data.U
      io.valid := true.B
    }
  }
  context.otherwise {
    io.out := 0.U
    io.valid := false.B
  }
}

class Decoder[T <: UInt](gen: T, code_file: String, len_file: String) extends Module {

  // bit mask for correctly comparing code buffer
  val mask = (x: Int) => ((1<<x)-1).U

  // scale for size of buffer
  val buffer_scale = 4

  // parse the code and len tables
  val codes = FileUtils.getLines(code_file)
  val lens  = FileUtils.getLines(len_file)

  // get data widths
  val code_width = lens.map(_.toInt).max
  val data_width = gen.getWidth
  val len_width  = ( log(code_width) / log(2.0) ).ceil.toInt
  val ptr_width  = ( log(buffer_scale*code_width) / log(2.0) ).ceil.toInt

  // get all the code, length, data tuples
  val codes_lens = (codes zip lens).zipWithIndex.map(x => (x._1._1.toInt, x._1._2.toInt, x._2.toInt)).sortWith((a,b) => a._2 < a._2)

  // create a buffer and pointer
  val code_buffer_data  = RegInit(0.U((buffer_scale*code_width).W))
  val code_buffer_ptr   = RegInit(0.U((ptr_width).W))

  // create a wire for the next buffer and pointer
  val next_code_buffer_data  = Wire(UInt((buffer_scale*code_width).W))
  val next_code_buffer_ptr   = Wire(UInt((ptr_width).W))


  // initialise IO
  val io = IO(new DecoderIO(gen))

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := true.B

  // registered input signals
  val input_bits  = io.in.bits
  val input_valid = io.in.valid

  // initialise decoder for each width
  val decoded_data  = RegInit(VecInit(Seq.fill(code_width)(0.U(code_width.W))))
  val decoded_valid = RegInit(VecInit(Seq.fill(code_width)(false.B)))

  // find out if any of the decoded words are valid
  val one_decoded_valid = decoded_valid.reduce(_|_)

  // create all the lookup tables
  for ( i <- 1 to code_width ) {
    // get the code data pairs
    val code_data = codes_lens.filter(_._2 == i).map(x => (x._1, x._3))
    // only create for lists with elements
    if (!code_data.isEmpty) {
      // create the LUT
      val lut = Module(new LookUpTable(UInt(i.W), code_data))
      // connect the input signal
      lut.io.in := next_code_buffer_data >> (next_code_buffer_ptr - i.U)
      // connect up the decoded data and valid signal
      decoded_data(i-1)   := lut.io.out
      decoded_valid(i-1)  := lut.io.valid && ( next_code_buffer_ptr >= (i-1).U )
    }
  }

  // find the smallest decoded value
  val index = PriorityEncoder(decoded_valid)

  // get the current code word length
  val curr_len = Wire(UInt(len_width.W))
  when (one_decoded_valid) {
    curr_len := index + 1.U
  } .otherwise {
    curr_len := 0.U
  }

  // create a state machine for the buffer
  val buffer_state = RegInit(BufferState.FILL)

  // a latch to detect the last output
  val last_buffer = RegInit(false.B)
  when(io.in.last) {
    last_buffer := true.B
  }

  // valid signal for the output
  val output_valid = RegInit(false.B)

  // initial values for next code buffer and pointer
  next_code_buffer_data  := code_buffer_data
  next_code_buffer_ptr   := code_buffer_ptr

  switch(buffer_state) {
    is(BufferState.FILL) {
      when(input_valid && io.out.ready) {
        // only update the code buffer if the output is ready and the input is valid
        next_code_buffer_data := ( code_buffer_data << data_width.U ) | input_bits
        next_code_buffer_ptr  := code_buffer_ptr + data_width.U - curr_len
        // set the output signal to valid
        output_valid := true.B
        when(code_buffer_ptr >= (2*code_width).U || io.in.last) {
          // if the code buffer is about to overflow, just output instead
          buffer_state := BufferState.FULL
          // set the ready signal
          io.in.ready := false.B
        } .otherwise {
          // otherwise, stay in the current state
          buffer_state := BufferState.FILL
          // set the ready signal
          io.in.ready := true.B
        }
      }.otherwise {
        // keep the code buffer and pointer the same
        next_code_buffer_data := code_buffer_data
        next_code_buffer_ptr  := code_buffer_ptr
        // update state
        buffer_state := BufferState.FILL
        // update valid signal
        output_valid := false.B
        // set the ready signal
        io.in.ready := true.B
      }
    }
    is(BufferState.FULL) {
      when (io.out.ready) {
        // shift out the code words in the buffer
        next_code_buffer_data := code_buffer_data
        next_code_buffer_ptr  := code_buffer_ptr - curr_len
        // update valid signal
        output_valid := true.B
        when(code_buffer_ptr >= (3*code_width).U || last_buffer) {
          buffer_state := BufferState.FULL
          io.in.ready := false.B
        } .otherwise {
          buffer_state := BufferState.FILL
          io.in.ready := true.B
        }
      } .otherwise {
        // keep the buffer the same
        next_code_buffer_data := code_buffer_data
        next_code_buffer_ptr  := code_buffer_ptr
        buffer_state := BufferState.FULL
        io.in.ready := false.B
        output_valid := false.B
      }
    }
  }

  // update code buffer and pointer
  code_buffer_data  := next_code_buffer_data
  code_buffer_ptr   := next_code_buffer_ptr

  // set the outputs based on the lowest index
  io.out.bits   := RegNext(decoded_data(index))
  io.out.valid  := RegNext(output_valid && one_decoded_valid)

  // last signal logic
  when(last_buffer && ( (code_buffer_ptr === 0.U) || !one_decoded_valid ) ) {
    io.out.last := true.B
  } .otherwise {
    io.out.last := false.B
  }

}

class BufferedDecoder[T <: UInt](gen: T, code_file: String, len_file: String) extends Module {

  // initialise IO
  val io = IO(new DecoderIO(gen))

  // initialise decoder
  val decoder = Module( new Decoder[T](gen, code_file, len_file) )

  // get the data width
  val data_width = gen.getWidth

  // create a queue for incoming samples
  val queue = Module(new Queue(Bits((data_width+1).W), 2)).io

  // connect the input of the queue
  queue.enq.bits := Cat(io.in.last, io.in.bits)
  queue.enq.valid := io.in.valid
  io.in.ready := queue.enq.ready

  // connect the output of the queue to the decoder
  decoder.io.in.bits  := queue.deq.bits
  decoder.io.in.valid := queue.deq.valid
  decoder.io.in.last  := queue.deq.bits >> data_width
  queue.deq.ready := decoder.io.in.ready

  // connect output directly
  io.out.bits   := decoder.io.out.bits
  io.out.valid  := decoder.io.out.valid
  io.out.last   := decoder.io.out.last
  decoder.io.out.ready := io.out.ready

}
