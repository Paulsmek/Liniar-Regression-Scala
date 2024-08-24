class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  override def toString: String = {
    data.map(_.mkString(",")).mkString("\n")
  }

  def selectColumn(col: String): Dataset = {
    val columnIndex = getHeader.indexOf(col)
    val selectedData = data.map(row => List(row(columnIndex)))
    new Dataset(selectedData)
  }

  def selectColumns(cols: List[String]): Dataset = {
    val columnIndices = cols.map(col => {
      val index = getHeader.indexOf(col)
      index
    })
    val selectedData = data.map(row => columnIndices.map(index => row(index)))
    new Dataset(selectedData)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData = data.tail.sortBy(row => row.head)
    val validationSize = math.ceil(1 / percentage).toInt - 1
    def distributeData(data: List[List[String]], validationSize: Int): (List[List[String]], List[List[String]]) = {
      val (training, validation) = data.zipWithIndex.partition {
        case (_, index) => index % (validationSize + 1) < validationSize }
      (training.map(_._1) , validation.map(_._1) )
    }
    val (trainingData, validationData) = distributeData(sortedData, validationSize)
    val trainingDataset = new Dataset(data.head :: trainingData)
    val validationDataset = new Dataset(data.head :: validationData)
    (trainingDataset, validationDataset)
  }


  def size: Int = data.size

  def getRows: List[List[String]] = data

  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val source = scala.io.Source.fromFile(csv_filename)
    val lines = try source.getLines().toList finally source.close()
    val data = lines.map(_.split(",").toList)
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
