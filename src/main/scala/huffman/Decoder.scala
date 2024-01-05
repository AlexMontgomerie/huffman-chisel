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

class LookUpTable[T <: UInt](gen: T, data_width: Int, code_data: Seq[(Int,Int)]) extends Module {

  // define the IO
  val io = IO(new Bundle {
    val in    = Input(gen)
    val out   = Output(UInt(data_width.W))
    val valid = Output(Bool())
  })

  // get the code width
  val code_width = gen.getWidth

  // generate the LUT
  var context = {
    val (code, data) = code_data.head
    when(io.in === Reverse(code.U(code_width.W))) {
      io.out := data.U
      io.valid := true.B
    }
  }
  for((code, data) <- code_data.tail) {
    context = context.elsewhen(io.in === Reverse(code.U(code_width.W))) {
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
  val buffer_scale = 5

  // parse the code and len tables
  val codes = FileUtils.getLines(code_file)
  val lens  = FileUtils.getLines(len_file)

  // get the min and max lengths
  val min_len = lens.map(_.toInt).min
  val max_len = lens.map(_.toInt).max

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

  // initialise IO
  val io = IO(new DecoderIO(gen))

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := true.B

  // registered input signals
  val input_bits  = Reverse(io.in.bits)
  val input_valid = io.in.valid

  // initialise decoder for each width
  val decoded_data  = RegInit(VecInit(Seq.fill(code_width)(0.U(code_width.W))))
  val decoded_valid = RegInit(VecInit(Seq.fill(code_width)(false.B)))

  // find out if any of the decoded words are valid
  val one_decoded_valid = decoded_valid.reduce(_|_)

  // find the smallest decoded value
  val index = PriorityEncoder(decoded_valid)

  // get the current code word length
  val curr_len = Wire(UInt(len_width.W))
  when (one_decoded_valid) {
    curr_len := index + 1.U
  } .otherwise {
    curr_len := 0.U
  }

  // get all code-data pairs
  val code_data_pairs = (1 to code_width).map(i => codes_lens.filter(_._2 == i).map(x => (x._1, x._3))).zipWithIndex

  // create all look-up tables
  val lut = code_data_pairs.filter(x => !x._1.isEmpty).map(x => (x._2, Module(new LookUpTable(UInt((x._2+1).W), data_width, x._1)).io))
  print(lut)

  // iterate over look up tables to intialise the decoded_data output
  lut.foreach {
    case(i, lut_hw) => {
      lut_hw.in := ( ( (input_bits << code_buffer_ptr) & mask(code_width*2) ) | code_buffer_data & mask(code_width*2) ) >> curr_len
      decoded_data(i)   := lut_hw.out
      // decoded_valid(i)  := lut_hw.valid && input_valid && (code_buffer_ptr > (i+1).U)
      decoded_valid(i)  := lut_hw.valid
    }
  }

  // create a state machine for the buffer
  val buffer_state = RegInit(BufferState.EMPTY)

  // a latch to detect the last output
  val last_buffer = RegInit(false.B)
  when(io.in.last) {
    last_buffer := true.B
  }

  // valid signal for the output
  val output_valid = RegInit(false.B)

  switch(buffer_state) {
    is(BufferState.EMPTY) {
      when(input_valid) {
        // get the next value
        val next_code_buffer_data = ( ( input_bits << code_buffer_ptr ) | code_buffer_data ) >> curr_len
        val next_code_buffer_ptr  = code_buffer_ptr + data_width.U
        // only update the code buffer
        code_buffer_data := next_code_buffer_data
        code_buffer_ptr  := next_code_buffer_ptr
        // set the output signal to valid
        io.in.ready := true.B
        when(next_code_buffer_ptr >= min_len.U) {
          buffer_state := BufferState.FILL
          output_valid := true.B
        } .otherwise {
          buffer_state := BufferState.EMPTY
          output_valid := false.B
        }
      }.otherwise {
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
        // get the next value
        val next_code_buffer_data = ( ( input_bits << code_buffer_ptr ) | code_buffer_data ) >> curr_len
        val next_code_buffer_ptr  = code_buffer_ptr + data_width.U - curr_len
        // only update the code buffer
        code_buffer_data := next_code_buffer_data
        code_buffer_ptr  := next_code_buffer_ptr
        // set the output signal to valid
        output_valid := true.B
        when(next_code_buffer_ptr > (2*max_len).U || io.in.last) {
          buffer_state := BufferState.FULL
          io.in.ready := false.B
        } .elsewhen(next_code_buffer_ptr < min_len.U) {
          buffer_state := BufferState.EMPTY
          io.in.ready := true.B
        } .otherwise {
          buffer_state := BufferState.FILL
          io.in.ready := true.B
        }
      }.otherwise {
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
      when (io.out.ready) {
        // get the next value
        val next_code_buffer_data =  code_buffer_data >> curr_len
        val next_code_buffer_ptr  = code_buffer_ptr - curr_len
        // only update the code buffer
        code_buffer_data := next_code_buffer_data
        code_buffer_ptr  := next_code_buffer_ptr
        // update valid signal
        output_valid := true.B
        when(next_code_buffer_ptr > (2*max_len).U || io.in.last || last_buffer) {
          buffer_state := BufferState.FULL
          io.in.ready := false.B
        } .otherwise {
          buffer_state := BufferState.FILL
          io.in.ready := true.B
        }
      } .otherwise {
        // keep the buffer the same
        code_buffer_data := code_buffer_data
        code_buffer_ptr  := code_buffer_ptr
        buffer_state := BufferState.FULL
        io.in.ready := false.B
        output_valid := false.B
      }
    }
  }

  // // update code buffer and pointer
  // code_buffer_data  := next_code_buffer_data
  // code_buffer_ptr   := next_code_buffer_ptr

  // set the outputs based on the lowest index
  io.out.bits   := decoded_data(index)
  io.out.valid  := output_valid && one_decoded_valid
  // io.out.valid  := false.B

  // io.out.bits   := RegNext(decoded_data(index))
  // io.out.valid  := RegNext(output_valid && one_decoded_valid)

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
