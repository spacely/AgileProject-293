package gps
import chisel3._
import chisel3.util._
import chisel3.experimental


class MatrixtransposeIO(p: GPSParams, rows: Int, cols: Int) extends Bundle {
    val in = Flipped(Decoupled(new Bundle {
        val a = Vec(p.rows, Vec(p.cols, experimental.FixedPoint(p.width.W, p.bp.BP)))
    }))
    val out = Decoupled(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    val state = Output(UInt(8.W))
    val counter = Output(UInt(8.W))
}

class MatrixTranspose(p: GPSParams, rows: Int, cols: Int) extends Module {
    // rows and cols sizes are for input dimensions
    require(cols == 4)
    require(rows >= 4)
    val io = IO(new MatrixtransposeIO(p, p.rows, p.cols))

    // calculate output matrix in zero time
    val c = Seq.tabulate(rows, cols) {
        (i,j) => io.in.bits.a(j)(i)
    }
    
    // assign output
    for( i <- 0 until p.rows) {
        for( j <- 0 until p.cols) {
            io.out.bits(i)(j) := c(i)(j)
        }
    }
  
}
