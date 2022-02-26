package gps
import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}
import spire.std.double

class MatrixtransposeIO(p: GPSParams) extends Bundle {
    val in = Flipped(Decoupled(new Bundle {
        val a = Vec(p.rows, Vec(p.cols, experimental.FixedPoint(p.width.W, p.bp.BP)))
    }))
    val out = Decoupled(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    //val state = Output(UInt(8.W))
    //val counter = Output(UInt(8.W))
}

class MatrixTranspose(p: GPSParams) extends Module {
    // rows and cols sizes are for input dimensions
    val io = IO(new MatrixtransposeIO(p))
    implicit val scale = p.scale
    io.in.ready := 1.B

    // calculate output matrix in zero time
    val c = Seq.tabulate(p.cols, p.rows) {
        (i,j) => io.in.bits.a(j)(i)
    }
    
    // assign output
    when(io.in.valid){
        io.out.valid := 1.B
        for( i <- 0 until p.cols) {
            for( j <- 0 until p.rows) {
                io.out.bits(i)(j) := c(i)(j)
            }
        }
    } .otherwise{
        io.out.valid := 0.B
        for( i <- 0 until p.cols) {
            for( j <- 0 until p.rows) {
                io.out.bits(i)(j) := p.fixedToChisel(FixedPoint(0.0))
            }
        }
    }
  
}
