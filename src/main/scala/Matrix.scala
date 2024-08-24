type Mat = List[List[Double]]

class Matrix(m: Option[Mat]) {

  def transpose: Matrix = {
    data match {
      case Some(matrixData) =>
        val numRows = matrixData.length
        val numCols = if (numRows > 0) matrixData.head.length else 0
        val isRectangular = matrixData.forall(row => row.length == numCols)
        if (!isRectangular) {
          Matrix(None)
        } else {
          val transposedData = Array.ofDim[Double](numCols, numRows)
          for {
            i <- 0 until numRows
            j <- 0 until numCols
          } {
            transposedData(j)(i) = matrixData(i)(j)
          }
          Matrix(Some(transposedData.map(_.toList).toList))
        }

      case None => Matrix(None)
    }
  }


  def map(f: Double => Double): Matrix = m match {
    case Some(matrix) => Matrix(Some(matrix.map(_.map(f))))
    case None => Matrix(None)
  }

  def *(other: Matrix): Matrix = {
    if (data.isEmpty || other.data.isEmpty) {
      Matrix(None)
    } else {
      val numRowsA = data.get.length
      val numColsA = data.get.headOption.map(_.length).getOrElse(0)
      val numRowsB = other.data.get.length
      val numColsB = other.data.get.headOption.map(_.length).getOrElse(0)
      if (numColsA != numRowsB) {
        Matrix(None)
      } else {
        val resultData = for {
          rowA <- data.get
        } yield {
          for {
            colB <- other.transpose.data.get
          } yield {
            rowA.zip(colB).map { case (a, b) => a * b }.sum
          }
        }
        Matrix(Some(resultData))
      }
    }
  }

  def ++(x: Double): Matrix = m match {
    case Some(matrix) => Matrix(Some(matrix.map(row => row :+ x)))
    case None => Matrix(None)
  }

  def -(other: Matrix): Matrix = {
    (data, other.data) match {
      case (Some(mat1), Some(mat2)) =>
        if (mat1.length != mat2.length || mat1.head.length != mat2.head.length) {
          Matrix(None)
        } else {
          val subtractedMat = mat1.zip(mat2).map { case (row1, row2) =>
            row1.zip(row2).map { case (elem1, elem2) =>
              elem1 - elem2
            }
          }
          Matrix(Some(subtractedMat))
        }
      case _ => Matrix(None)
    }
  }


  def data: Option[Mat] = m
  def height: Option[Int] = data.map(_.length)
  def width: Option[Int] = data.map(_.headOption.map(_.length).getOrElse(0))


  override def toString: String = m match {
    case Some(matrix) => matrix.map(_.mkString("\t")).mkString("\n")
    case None => "Error"
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)

  def apply(dataset: Dataset): Matrix = {
    val data = dataset.getRows.tail
    val matrixData = data.map(_.map(_.toDouble))
    Matrix(Some(matrixData))
  }

}
