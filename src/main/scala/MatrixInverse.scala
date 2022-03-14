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
        val a_signed = a.asSInt
        val b_signed = b.asSInt
        
        val zero = Wire(UInt(p.bp.W))
        zero := 0.U
        val a_signed_extend = Cat(a_signed, zero).asSInt
        val z = (a_signed_extend/b_signed).asFixedPoint(p.bp.BP)
        val out = z(p.width-1, 0).asFixedPoint(p.bp.BP)
        out
    }
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

class MatrixInverseIO(p: GPSParams) extends Bundle {
    val in = Flipped(Decoupled(new Bundle {
        val a = Vec(p.rows, Vec(p.cols, experimental.FixedPoint(p.width.W, p.bp.BP)))
    }))
    val out = Decoupled(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    // val debug_A = Output(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    // val debug_Y = Output(Vec(p.cols, Vec(p.rows, experimental.FixedPoint(p.width.W, p.bp.BP))))
    // val debug_ratio_i = Output(UInt())
    // val debug_ratio_j = Output(UInt())
    // val debug_ratio_k = Output(UInt())
    // val debug_ratio_l = Output(UInt())
    val state = Output(UInt(8.W))
}

class MatrixInverse(p: GPSParams) extends Module {

    // rows and cols sizes are for input dimensions
    val io = IO(new MatrixInverseIO(p))
    implicit val scale = p.scale

    import MatrixInverse._
    val state = RegInit(idle)
    state := state
    io.state := state
    io.in.ready := 0.B

    val ident = Seq.tabulate(p.rows,p.cols) { (i,j) => if (i==j) p.fixedToChisel(FixedPoint(1)) else p.fixedToChisel(FixedPoint(0)) }
    val A = Reg(Vec(p.cols, Vec(p.rows, experimental.FixedPoint((p.width).W, p.bp.BP))))
    val Y = Reg(Vec(p.cols, Vec(p.rows, experimental.FixedPoint((p.width).W, p.bp.BP))))
    val X = Reg(Vec(p.cols, Vec(p.rows, experimental.FixedPoint((p.width).W, p.bp.BP))))

    val ratio_i = RegInit(0.U(8.W))
    val ratio_j = RegInit(0.U(8.W))
    val ratio_k = RegInit(0.U(8.W))
    val ratio_l = RegInit(0.U(8.W))
    val ratio_j_1 = RegInit(0.U(8.W))
    val ratio_j_2 = RegInit(0.U(8.W))
    val ratio_j_3 = RegInit(0.U(8.W))
    val ratio =  Reg(experimental.FixedPoint((p.width).W, p.bp.BP))
    ratio := ratio
    io.out.valid := false.B
    switch(state) {
        is(idle) {
            io.in.ready := 1.B
            ratio_i := 0.U
            ratio_j := 1.U
            ratio_k := 1.U
            ratio_l := 0.U
            when(io.in.valid === true.B){
                state := divide
                for (i <- 0 until p.rows) {
                    for (j <- 0 until p.cols) {
                        Y(i)(j) := ident(i)(j)
                    }
                }
                A := io.in.bits.a
                ratio :=A(ratio_j)(ratio_i)
                A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
            }
        }
        is(divide){
            ratio := ratio
            when(ratio_j < A.length.U){
                when(ratio_k < A.length.U) {
                    ratio_k := ratio_k + 1.U
                    A(ratio_j)(ratio_k):= A(ratio_j)(ratio_k).asFixedPoint(p.bp.BP) -% ratio*A(ratio_i)(ratio_k).asFixedPoint(p.bp.BP)
                }
                when(ratio_l < Y.length.U) {
                    ratio_l := ratio_l + 1.U
                    Y(ratio_j)(ratio_l) := Y(ratio_j)(ratio_l).asFixedPoint(p.bp.BP) -% ratio*Y(ratio_i)(ratio_l).asFixedPoint(p.bp.BP)
                }
            }
            when(ratio_k >=  (A.length-1).U && ratio_l >= (Y.length-1).U) {                
                when(ratio_j < (A.length-1).U ) {
                    A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
                    ratio_j := ratio_j + 1.U
                    ratio_k := ratio_i + 1.U
                    ratio_l := 0.U
                    ratio := fixedDiv(p, A(ratio_j)(ratio_i),A(ratio_i)(ratio_i)).asFixedPoint(p.bp.BP)
                } .otherwise {

                    ratio_k := ratio_i + 2.U
                    ratio_j := ratio_i + 2.U
                    ratio_i := ratio_i + 1.U
                    ratio_l := 0.U
                    ratio := fixedDiv(p, A(ratio_j)(ratio_i),A(ratio_i)(ratio_i)).asFixedPoint(p.bp.BP)
                    when(ratio_i + 2.U === A.length.U){
                        state := compute
                        ratio_i := A.length.U
                        ratio_j := 0.U
                        ratio_k := 0.U
                        ratio_l := 0.U
                    }.otherwise{
                        ratio := fixedDiv(p, A(ratio_j)(ratio_i),A(ratio_i)(ratio_i)).asFixedPoint(p.bp.BP)
                        A(ratio_j)(ratio_i) := p.fixedToChisel(FixedPoint(0.0))
                    }
                }
            }
        }
        is(compute){
            when(ratio_i-1.U >= 0.U)
            {
                when(ratio_j_1 < X(0).length.U) {
                    X(ratio_i)(ratio_j_1) := Y(ratio_i)(ratio_j_1)
                    ratio_j_1 := ratio_j_1 + 1.U
                } .elsewhen(ratio_j_2 < A.length.U) {
                    when(ratio_l < Y.size.U){
                        X(ratio_i)(ratio_l) := X(ratio_i)(ratio_l).asFixedPoint(p.bp.BP) -% A(ratio_i)(ratio_j_2).asFixedPoint(p.bp.BP)*X(ratio_j_2)(ratio_l).asFixedPoint(p.bp.BP)
                    }
                } .elsewhen(ratio_j_3 < X(0).length.U) {
                    X(ratio_i)(ratio_j) := fixedDiv(p, X(ratio_i)(ratio_j).asFixedPoint(p.bp.BP), A(ratio_i)(ratio_i)).asFixedPoint(p.bp.BP)
                } .otherwise {
                    ratio_i := ratio_i - 1.U
                }
            } .otherwise {
                state := idle
                io.out.valid := 1.B
            }
        }
    }
    
    // assign output

    for( i <- 0 until p.cols) {
        for( j <- 0 until p.rows) {
            io.out.bits(i)(j) := X(i)(j)
        }
    }
    // for( i <- 0 until p.cols) {
    //     for( j <- 0 until p.rows) {
    //         io.debug_A(i)(j) := A(i)(j)
    //     }
    // }
    // for( i <- 0 until p.cols) {
    //     for( j <- 0 until p.rows) {
    //         io.debug_Y(i)(j) := Y(i)(j)
    //     }
    // }
    // io.debug_ratio_i := ratio_i
    // io.debug_ratio_j := ratio_j
    // io.debug_ratio_k := ratio_k
    // io.debug_ratio_l := ratio_l
   
  
}