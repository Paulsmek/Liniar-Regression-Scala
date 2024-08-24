import java.lang.System.load
import scala.annotation.tailrec
import scala.util.Random

object Regression {

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {
    val (train, test) = Dataset.apply(dataset_file).split(test_percentage)

    //matrici la care se adauga coloana de 1
    val train_mat = Matrix.apply(train.selectColumns(attribute_columns)).++(1)
    val test_mat = Matrix.apply(test.selectColumns(attribute_columns)).++(1)

    //matrici care contin coloana folosita ca variabila tinta
    val value_train_mat = Matrix.apply(train.selectColumn(value_column))
    val value_test_mat = Matrix.apply(test.selectColumn(value_column))

    val trainer: Matrix = Matrix.apply(List.fill(train_mat.width.get)(List(0.0)))

    @tailrec
    def gradientDesc(dataset: Matrix, Mat: Matrix, steps: Int): Matrix = {
      if (steps == gradient_descent_steps) Mat
      else {
        val err = dataset.*(Mat).- (value_train_mat)
        val a = dataset.transpose * err
        val grad = a.map(_ / dataset.height.get)
        gradientDesc(dataset, Mat.- (grad.map(_ * alpha)), steps + 1)
      }

    }

    val output = gradientDesc(train_mat, trainer, 0)

    val error = test_mat.* (output).- (value_test_mat)
    val absoluteError = error.map(math.abs)
    val totalError = absoluteError.data.get.flatten.sum / absoluteError.height.get

    (output, totalError)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }

}
