package huffman

import math._
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType

import chisel3.experimental.{annotate, ChiselAnnotation}
// import firrtl.annotations.MemorySynthInit

class EncoderIO[T <: UInt](gen: T) extends Bundle {
  val in  = Flipped(Stream(gen))
  val out = Stream(gen)
}

class Encoder[T <: UInt](gen: T, size: Int, code_width: Int, code_file: String, len_file: String) extends Module {

  // // update annotations to allow FPGA synthesis
  // annotate(new ChiselAnnotation {
  //   override def toFirrtl =
  //     MemorySynthInit
  // })

  // get data widths
  val data_width = gen.getWidth
  val len_width  = ( log(code_width) / log(2.0) ).ceil.toInt
  val ptr_width  = ( log(2*code_width) / log(2.0) ).ceil.toInt

  // initialise IO
  val io = IO(new EncoderIO(gen))

  // create an instance of memory for both codes and their length
  val code_mem = SyncReadMem(pow(2,data_width).toInt, UInt(code_width.W))
  val len_mem  = SyncReadMem(pow(2,data_width).toInt, UInt(len_width.W))

  // create a buffer and pointer
  val code_buffer   = RegInit(0.U((2*code_width).W))
  val code_pointer  = RegInit(0.U((ptr_width).W))

  // load the buffers from a file
  if (code_file.trim().nonEmpty) {
    loadMemoryFromFileInline(code_mem, code_file, hexOrBinary=MemoryLoadFileType.Binary)
  }
  if (len_file.trim().nonEmpty) {
    loadMemoryFromFileInline(len_mem, len_file, hexOrBinary=MemoryLoadFileType.Binary)
  }

  // set defaults for IO
  io.out.bits   := DontCare
  io.out.valid  := false.B
  io.out.last   := false.B
  io.in.ready   := false.B

  // get the code word and length
  val curr_code = code_mem.read(io.in.bits, io.in.valid)
  val curr_len  = len_mem.read(io.in.bits, io.in.valid)

  // code buffer logic
  when ( RegNext(io.in.valid) && io.out.ready ) {
    // only update the buffer and pointer if the input is valid and the output
    // is ready
    when ( code_pointer >= data_width.U ) {
      // shorten when the pointer is past the data width
      code_buffer   := ( code_buffer | ( curr_code << code_pointer ) ) >> data_width.U
      code_pointer  := code_pointer + curr_len - data_width.U
    } .otherwise {
      // append the codes
      code_buffer   := code_buffer | ( curr_code << code_pointer )
      code_pointer  := code_pointer + curr_len
    }
  } .otherwise {
    // keep the code buffer as is
    code_buffer   := code_buffer
    code_pointer  := code_pointer
  }

  // connect ready signal straight through
  io.in.ready := io.out.ready

  // connect lower code_buffer bits to the output
  io.out.bits   := code_buffer

  // define output valid signal logic
  when ( ( RegNext(io.in.last) && ( code_pointer < data_width.U ) ) || ( code_pointer >= data_width.U ) ) {
    io.out.valid := true.B
  } .otherwise {
    io.out.valid := false.B
  }

  // define output last signal logic
  when ( RegNext(io.in.last) && ( code_pointer < data_width.U ) ) {
    io.out.last := true.B
  } .otherwise {
    io.out.last   := RegNext(RegNext(io.in.last))
  }
}

class BufferedEncoder[T <: UInt](gen: T, size: Int, code_width: Int, code_file: String, len_file: String) extends Module {

  // initialise IO
  val io = IO(new EncoderIO(gen))

  // initialise encoder
  val encoder = Module( new Encoder[T](gen, size, code_width, code_file, len_file) )

  // connect inputs with registers
  encoder.io.in.bits  := RegNext(io.in.bits)
  encoder.io.in.valid := RegNext(io.in.valid)
  encoder.io.in.last  := RegNext(io.in.last)
  io.in.ready := encoder.io.in.ready

  // connect output directly
  io.out.bits   := encoder.io.out.bits
  io.out.valid  := encoder.io.out.valid
  io.out.last   := encoder.io.out.last
  encoder.io.out.ready := io.out.ready

}
