package gps
import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}
import spire.std.double


object MatrixInverse {
  val idle :: divide :: compute  :: Nil= Enum(3)

  def fixedDiv(p: GPSParams, a: experimental.FixedPoint, b: experimental.FixedPoint): experimental.FixedPoint = {
        val a_s = a.litValue
        val b_s = b.litValue
        
        val z = experimental.FixedPoint((a_s/b_s), p.width.W, p.bp.BP)
        z
    }
}

class MatrixInverseIO(p: GPSParams) extends Bundle {
    val in = Flipped(Decoupled(new Bundle {
        val a = Vec(p.rows, Vec(p.cols, experimental.FixedPoint(p.width.W, p.bp.BP)))
    }))
    val out = Decoupled(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    val state = Output(UInt(8.W))
    //val counter = Output(UInt(8.W))
}

class MatrixDivideIO(p: GPSParams) extends Bundle {
    val a = Input(experimental.FixedPoint(p.width.W, p.bp.BP))
    val b = Input(experimental.FixedPoint(p.width.W, p.bp.BP))
    val z = Output(experimental.FixedPoint(p.width.W, p.bp.BP))

}

class MatrixDivide(p: GPSParams) extends Module {
    val io = IO(new MatrixDivideIO(p))
    import MatrixInverse.fixedDiv
    
    io.z := fixedDiv(p, io.a, io.b)
}

class MatrixInverse(p: GPSParams) extends Module {

    // rows and cols sizes are for input dimensions
    val io = IO(new MatrixInverseIO(p))
    implicit val scale = p.scale

    import MatrixInverse._
    val state = RegInit(idle)
    io.in.ready := 0.B

    val ident = Seq.tabulate(p.rows,p.cols) { (i,j) => if (i==j) p.fixedToChisel(FixedPoint(1)) else p.fixedToChisel(FixedPoint(0)) }
    val A = Reg(Vec(p.cols, Vec(p.rows, experimental.FixedPoint())))
    val Y = Reg(Vec(p.cols, Vec(p.rows, experimental.FixedPoint())))

    val ratio_i = RegInit(0.U)
    val ratio_j = RegInit(0.U)
    val ratio_k = RegInit(0.U)
    val ratio_l = RegInit(0.U)

    val ratio = RegInit(p.fixedToChisel(FixedPoint(0.0)))
    switch(state) {
        is(idle) {
            io.in.ready := 1.B
            ratio_i := 0.U
            ratio_j := 0.U
            ratio_k := 0.U
            ratio_l := 0.U
            when(io.in.valid){
                state := divide
                for (i <- 0 until p.rows) {
                    for (j <- 0 until p.cols) {
                        Y(i)(j) := ident(i)(j)
                    }
                }
                A := io.in.bits.a
                ratio := (A(ratio_j)(ratio_i)/A(ratio_i)(ratio_i))
                A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
            }
        }
        is(divide){

                    when(ratio_k < A.length.U) {
                        ratio_k := ratio_k + 1.U
                        A(ratio_j)(ratio_k) := A(ratio_j)(ratio_k) - ratio*A(ratio_i)(ratio_k)
                    }
                    when(ratio_l < Y.length.U) {
                        ratio_l := ratio_l + 1.U
                        Y(ratio_j)(ratio_l) := Y(ratio_j)(ratio_l) - ratio*Y(ratio_i)(ratio_l)
                    }
                    when(ratio_k ===  A.length.U && ratio_l === Y.length.U) {
                        ratio := (A(ratio_j)(ratio_i)/A(ratio_i)(ratio_i))
                        
                        when(ratio_j < A.length.U) {
                            A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
                            ratio_j := ratio_j + 1.U
                            ratio_k := ratio_i + 1.U
                        } .otherwise {

                            ratio_k := ratio_i + 2.U
                            ratio_j := ratio_i + 2.U
                            ratio_i := ratio_i + 1.U
                            when(ratio_i + 1.U === A.length.U){
                                state := compute
                            }.otherwise{
                                A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
                            }
                        }
                    }

        }
        is(compute){

        }
    }
        
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