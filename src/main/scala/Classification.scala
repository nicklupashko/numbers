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

  val clusterList: List[String] = (0 until numberOfElements).map(_.toString).toList

  def distanceMatrix(arr: Array[Array[Double]]): Array[Array[Double]] = {
    val N: Int = arr.length
    val M: Int = arr(0).length
    val distMatrix: Array[Array[Double]] = Array.ofDim(M, M)
    val weights: Array[Double] = Array.fill(N)(Random.nextDouble)

    for (i <- 0 until M; j <- 0 until M)
      distMatrix(i)(j) = Math.sqrt((for (l <- 0 until N) yield
        weights(l) * Math.pow(arr(l)(i) - arr(l)(j), 2)).sum)

    distMatrix
  }

  def minDistRowColumn(arr: Array[Array[Double]]): (Double, Int, Int) = {
    (for (row <- arr.indices; col <- arr(row).indices if row != col)
      yield (arr(row)(col), row, col)).minBy(_._1)
  }

  def addColumn(arr: Array[Array[Double]], col: Array[Double]): Array[Array[Double]] = {
    (for (i <- arr.indices) yield arr(i) :+ col(i)).toArray
  }

  def joinColumns(arr: Array[Array[Double]], col1: Int, col2: Int): Array[Double] = {
    (for (i <- arr.indices) yield Math.max(arr(i)(col1), arr(i)(col2))).toArray
  }

  def removeColumns(arr: Array[Array[Double]], col1: Int, col2: Int): Array[Array[Double]] = {
    (for (i <- arr.indices) yield
      (for (j <- arr(i).indices if j != col1 && j != col2) yield arr(i)(j)).toArray
      ).toArray
  }

  def removeColumns(list: List[String], col1: Int, col2: Int): List[String] = {
    (for (i <- list.indices if i != col1 && i != col2)
      yield list(i)).toList :+ s"${list(col1)}, ${list(col2)}"
  }

  def clustering(data: Array[Array[Double]], clusters: List[String],
                 numberOfClusters: Int = 2): (Array[Array[Double]], List[String]) = {
    val (_, r, c) = minDistRowColumn(distanceMatrix(data))
    val newDataSet = addColumn(removeColumns(data, r, c), joinColumns(data, r, c))
    val newClusters = removeColumns(clusters, r, c)

    if (newDataSet(0).length == numberOfClusters) (newDataSet, newClusters)
    else clustering(newDataSet, newClusters, numberOfClusters)
  }

  def main(args: Array[String]): Unit = {
    val (data, clusters) = clustering(dataSet, clusterList)

    println("Data: ")
    data.foreach(row => println(row.mkString(", ")))

    println("Clusters: ")
    clusters.foreach(println)
  }
}
