package gps
import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

object MatMulModel {
  type Matrix = Seq[Seq[FixedPoint]]
  

  def apply(p: GPSParams, a: Matrix, b: Matrix): Matrix = {
    implicit val scale = p.scale
    val c = Seq.tabulate(a.length,b(0).size) {
      (i,j) => {
        var sum = FixedPoint(0)
        for(k <- 0 until a(0).size) {
          sum = sum + a(i)(k) * b(k)(j)
        }
        sum
      }
    }
    c
  }
}