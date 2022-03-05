package gps
import scala.collection.mutable.ArrayBuffer
import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

class MatrixInverseModel(val p: GPSParams) {
    type Matrix = Seq[Seq[FixedPoint]]
    implicit val scale = p.scale
    
    def genIdentity(n: Int): Matrix = Seq.tabulate(n,n) { (i,j) => if (i==j) FixedPoint(1) else FixedPoint(0) }
    def append(a: Matrix, b: Matrix): Matrix = {
        assert(a.size == b.size)
        assert(a.head.size == b.head.size)
        val c: Matrix = Seq.tabulate(a.head.size, a.size + b.size) {
            (i,j) => {
                if(j < a.size) a(i)(j)
                else b(i)(j-a.size)
            }
        }
        c
    }
    def gaussElimination(a: Matrix, y: Matrix): Matrix = {
        // based on: https://github.com/mazurkiewiczp/GaussS/blob/master/gauss.scala
        // TODO: current off by 1 error in output
        val A = ArrayBuffer.fill(a.size)(ArrayBuffer.fill(a.size)(FixedPoint(0.0)))
        for (i <- 0 until a.size) {
            for (j <- 0 until a.size) {
                A(i)(j) = a(i)(j)
            }
        }
        val Y = ArrayBuffer.fill(y.size)(ArrayBuffer.fill(y.size)(FixedPoint(0.0)))
        for (i <- 0 until y.size) {
            for (j <- 0 until y.size) {
                Y(i)(j) = y(i)(j)
            }
        }
        for(i <- 0 until A.length){
            for(j <- i+1 until A.length){
                var ratio = FixedPoint(0.0)
                ratio = (A(j)(i)/A(i)(i))
                A(j)(i) = FixedPoint(0.0)			
                for(k <- i+1 until A.length){
                    A(j)(k) -= ratio*A(i)(k)
                }
                for( l <- 0 until y.size){
                    Y(j)(l) -= ratio*Y(i)(l)
                }
            }
        }

        val X = ArrayBuffer.fill(y.size)(ArrayBuffer.fill(y.size)(FixedPoint(0.0)))
        for(i <- A.length-1 to 0 by -1){
            for (j <- 0 until X(0).length) {
                X(i)(j) = Y(i)(j)
            }
            for(j <- i+1 until A.length) {
                for(l <- 0 until y.size) {
                    X(i)(l) -= A(i)(j)*X(j)(l)
                }
            }
            for (j <- 0 until X(0).length) {
                X(i)(j) /= A(i)(i)
            }
        }
        val x = Seq.tabulate(y.size, a.size ) {
            (i,j) => X(i)(j)
        }
        //append(genIdentity(2),x)
        x
    }

    def cholesky(a: Matrix): Matrix = {
        // based on C implemenation https://github.com/PetteriAimonen/libfixmatrix/blob/master/fixmatrix.c

        var row, column, k = 0
        val A = ArrayBuffer.fill(a.size)(ArrayBuffer.fill(a.size)(FixedPoint(0.0)))
        
        for (row <- 0 until a(0).size )
        {
            for (column <- 0 until a.size )
            {
                if (row == column)
                {

                    var value = a(row)(column)
                    for (k <- 0 until column)
                    {
                        var Ljk = A(row)(k)
                        Ljk = Ljk * Ljk
                        value = value - Ljk
                    }
                    
                    A(row)(column) = value.sqrt;
                }
                else if (row < column)
                {
                    A(row)(column) = FixedPoint(0)
                    }
                else
                {
                    var value = a(row)(column)
                    for (k <- 0 until column)
                    {
                        var Lik = A(row)(k)
                        var Ljk = A(column)(k)
                        var product = Lik * Ljk
                        value = value - product
                    }
                    var Ljj = A(column)(column)
                    value = value / Ljj
                    A(row)(column) = value
                    }
            }
        }
        val x = Seq.tabulate(A(0).size, A.size ) {
            (i,j) => A(i)(j)
        }
        x
        
        
    }

    def choleskyWikipedia(a: Matrix): Matrix = {
    // based on wikipedia psuedo code
        val L = ArrayBuffer.fill(a.size)(ArrayBuffer.fill(a.size)(FixedPoint(0.0)))

        for(i <- 0 until a.size) 
        {
            for (j <- 0 to i) 
            {
                var sum = FixedPoint(0);
                for (k <- 0 until j)
                {
                    sum += L(i)(k) * L(j)(k);
                }
                if (i == j)
                {
                    L(i)(j) = sqrt(a(i)(i) - sum)
                }
                else
                    L(i)(j) = (1.0 / L(j)(j) * (a(i)(j) - sum))
            }
        }
        val x = Seq.tabulate(L(0).size, L.size ) {
            (i,j) => L(i)(j)
        }
        x
    }

}

