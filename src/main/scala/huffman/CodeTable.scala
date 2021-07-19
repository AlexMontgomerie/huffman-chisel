package huffman

import chisel3._
import chisel3.util._

class CodeTableIO[T <: Bits](gen: T, addr_width: Int) extends Bundle {
  val wr    = Input(Bool())
  val en    = Input(Bool())
  val waddr = Input(UInt(addr_width.W))
  val raddr = Input(UInt(addr_width.W))
  val in    = Input(gen)
  val out   = Output(gen)
  val in_w  = Input(gen)
  val out_w = Output(gen)
}

class CodeTable[T <: Bits](gen: T, size: Int) extends Module {

  // get both data and address widths
  val data_width = gen.getWidth
  val addr_width = (ln(size)/ln(2)).toInt
  val len_width  = (ln(data_width)/ln(2)).toInt

  // IO definition
  val io = IO(new CodeTableIO(gen, addr_width))

  // create an instance of memory for both codes and their length
  val code_mem  = SyncReadMem(size, gen)
  val width_mem = SyncReadMem(size, gen)

  // read and write logic
  when ( io.wr ) {
    val code_port  = code_mem(io.waddr)
    val width_port = width_mem(io.waddr)
    code_port  := io.in
    width_port := io.in_w
  }
  when ( io.en ) {
    val code_port  = code_mem(io.waddr)
    val width_port = width_mem(io.waddr)
    io.out   := code_port
    io.out_w := width_port
  }
}

