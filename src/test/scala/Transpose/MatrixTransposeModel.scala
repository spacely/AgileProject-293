package gps
import spire._
import spire.math._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

object MatrixTransposeModel {
  type Matrix = Seq[Seq[FixedPoint]]

  def apply(a: Matrix): Matrix = {
    val c = Seq.tabulate(a.head.size, a.size ) {
      (i,j) => a(j)(i)
    }
    c
  }
}
