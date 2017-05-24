import scala.util.Random

object Classification {
  val numberOfFeatures = 5
  val numberOfElements = 10

  val dataSet: Array[Array[Double]] = Array.ofDim[Double](numberOfFeatures, numberOfElements)
  dataSet(0) = Array(1.23,   1.04,  1.8,    0.43,  0.88,  0.57,  1.72,   1.7,   0.84,  0.6)
  dataSet(1) = Array(0.23,   0.39,  0.43,   0.18,  0.15,  0.34,  0.38,   0.09,  0.14,  0.21)
  dataSet(2) = Array(1.45,   1.3,   1.37,   1.65,  1.91,  1.68,  1.94,   1.89,  1.94,  2.06)
  dataSet(3) = Array(167.69, 186.1, 220.45, 169.3, 39.53, 40.41, 102.96, 37.02, 45.74, 40.07)
  dataSet(4) = Array(17.72,  18.39, 26.46,  22.37, 28.13, 17.55, 21.92,  19.52, 23.99, 21.76)

  val clusters: Map[Int, String] =
    (0 until numberOfFeatures).map(n => n -> n.toString).toMap

  def distanceMatrix(arr: Array[Array[Double]]): Array[Array[Double]] = {
    val N: Int = arr.length
    val M: Int = arr(0).length
    val distMatrix: Array[Array[Double]] = Array.ofDim(M, M)
    val weights: Array[Double] = Array.fill(M)(Random.nextDouble)

    for (i <- 0 until M; j <- 0 until M)
      distMatrix(i)(j) = Math.sqrt((for (l <- 0 until N)
        yield weights(l) * Math.pow(arr(l)(i) - arr(l)(j), 2)).sum)

    distMatrix
  }

  def minDistRowColumn(arr: Array[Array[Double]]): (Double, Int, Int) = {
    (for (row <- arr(0).indices; col <- arr(0).indices if row != col)
      yield (arr(row)(col), row, col)).minBy(_._1)
  }

  def main(args: Array[String]): Unit = {
    println("result: ")
  }
}
