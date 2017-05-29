import scala.io.Source
import scala.util.Random

object Classification {

  val dataSet: Array[Array[Double]] = dataSetFromFile("data10.txt")

  val clusterList: List[String] = dataSet.head.indices.map(_.toString).toList

  val weights: Array[Double] = Array.fill(dataSet.length)(Random.nextDouble)

  def dataSetFromFile(filePath: String): Array[Array[Double]] = {
    val regex = """\d+(\.\d+)?""".r
    Source.fromFile(filePath).getLines().toList
      .map(regex.findAllIn(_).map(_.toDouble).toArray).toArray
  }

  def distanceMatrix(arr: Array[Array[Double]]): Array[Array[Double]] = {
    val N: Int = arr.length
    val M: Int = arr(0).length
    val distMatrix: Array[Array[Double]] = Array.ofDim(M, M)

    for (i <- 0 until M; j <- (i + 1) until M)
      distMatrix(i)(j) = Math.sqrt((for (l <- 0 until N) yield
        weights(l) * Math.pow(arr(l)(i) - arr(l)(j), 2)).sum)

    distMatrix
  }

  def minDistanceRowColumn(arr: Array[Array[Double]]): (Double, Int, Int) = {
    (for (row <- arr.indices; col <- arr(row).indices if col > row)
      yield (arr(row)(col), row, col)).minBy(_._1)
  }

  def addColumn(arr: Array[Array[Double]], col: Array[Double]): Array[Array[Double]] = {
    (for (i <- arr.indices) yield arr(i) :+ col(i)).toArray
  }

  def joinedColumn(arr: Array[Array[Double]], col1: Int, col2: Int): Array[Double] = {
    (for (i <- arr.indices) yield Math.max(arr(i)(col1), arr(i)(col2))).toArray
  }

  def removeColumns(arr: Array[Array[Double]], col1: Int, col2: Int): Array[Array[Double]] = {
    (for (i <- arr.indices) yield
      (for (j <- arr(i).indices if j != col1 && j != col2)
        yield arr(i)(j)).toArray).toArray
  }

  def removeColumns(list: List[String], col1: Int, col2: Int): List[String] = {
    (for (i <- list.indices if i != col1 && i != col2)
      yield list(i)).toList :+ s"${list(col1)}, ${list(col2)}"
  }

  def clustering(data: Array[Array[Double]], clusters: List[String],
                 numberOfClusters: Int = 2): (Array[Array[Double]], List[String]) = {
    if (clusters.length <= numberOfClusters)
      (data, clusters)
    else {
      val (_, r, c) = minDistanceRowColumn(distanceMatrix(data))
      val newDataSet = addColumn(removeColumns(data, r, c), joinedColumn(data, r, c))
      val newClusters = removeColumns(clusters, r, c)
      clustering(newDataSet, newClusters, numberOfClusters)
    }
  }

  def main(args: Array[String]): Unit = {
    val (data, clusters) = clustering(dataSet, clusterList)

    println("Data: ")
    data.foreach(row => println(row.mkString(", ")))

    println("Clusters: ")
    clusters.foreach(println)
  }
}
