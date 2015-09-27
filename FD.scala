package mSketch
import Jama.Matrix

class FD {
  
  
  def FFD(mat: Matrix, l: Int): Matrix = {
    val cols = mat.getColumnDimension
    val rows = mat.getRowDimension
    
    var transpose = false
    if(l > cols) throw new IllegalArgumentException
    if(2 * l < cols) transpose = true
    val B = new Matrix(2 * l, cols)
    val Z = new Matrix(l, cols)
    var Brow = 1
    B.setMatrix(Array(0), 0, cols - 1, mat.getMatrix(Array(1), 0, cols - 1))


    for(i <- 1 to (rows - 1)){
      if(Brow % (2 * l) != 0) {
        B.setMatrix(Array(Brow), 0, cols - 1, mat.getMatrix(Array(i), 0, cols - 1))
        Brow += 1
        //println(B.print(1,1))
      } else {
        Brow = l - 1
        //println(B.print(1,1))
        val svd = B.svd
        //println(svd.getS.print(1,1))
        val d = math.pow(svd.getS.get(l - 1, l - 1), 2) // Last Singular Value
        val I = Matrix.identity(svd.getS.getRowDimension, svd.getS.getColumnDimension).times(d) //dI
        val S2 = sqDiag(svd.getS)
        B.setMatrix(0, l - 1, 0, cols - 1, maximum(S2.minus(I), 0).times(svd.getV.transpose))
        B.setMatrix(l, 2 * l - 1, 0, cols - 1, Z)
        //if(rows > 100 && i % (mat.getRowDimension * .1).toInt == 0) println(i + " / " + rows)
      }

    }

    B
  }
  
  private def sqrtDiag(D: Matrix): Matrix = {
    for(i <- 0 to (D.getRowDimension - 1))
        D.set(i, i, math.pow(D.get(i, i), .5))
    D
  }

   private def sqDiag(D: Matrix): Matrix = {
    for(i <- 0 to (D.getRowDimension - 1))
      D.set(i, i, math.pow(D.get(i, i), 2))
    D
  }

  private def maximum(D: Matrix, m: Double): Matrix = {
    for(i <- 0 to (D.getRowDimension - 1)){
      if(D.get(i,i) > m) D.set(i, i, math.pow(D.get(i, i), .5))
      else D.set(i, i, math.pow(m, .5))
    }

    D
  }

}