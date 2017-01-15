package sGeneticAlgorithm.ga.pareto

import sGeneticAlgorithm.ga.GAException

import scala.math.Numeric._


object ParetoFitness {

  def dominates[T](v1: Seq[T], v2: Seq[T])(implicit numeric: Numeric[T]): Boolean = {
    import numeric._
    if (v1.size != v2.size) throw new GAException("Cannot compare vectors of unequal sizes")
    val s = v1.size
    def dominates(i: Int): Boolean = {
      v2(i) >= v1(i) match {
        case true => false
        case false =>
          i >= s-1 match {
            case true => true
            case false => dominates(i + 1)
          }
      }
    }
    dominates(0)
  }

  def dominanceMatrix[T](fitnessVectors: Seq[Seq[T]])(implicit numeric: Numeric[T]): Vector[Vector[Int]] = {
    val s = fitnessVectors.size - 1
    (for (i <- 0 to s) yield {
      (for (j <- 0 to s) yield {
        i == j match {
          case true => 0
          case false =>
            dominates (fitnessVectors (i), fitnessVectors (j) ) match {
            case true => 1
            case false => 0
            }
        }
      }).toVector
    }).toVector
  }

  def strengthFromMatrix[T](dMatrix: Vector[Vector[Int]]): Vector[Int] = {
    dMatrix.map(_.sum)
  }

  def getCol(n: Int, a: Vector[Vector[Int]]) = a.map{_(n)}

  def ranks[T](dominanceMatrix: Vector[Vector[Int]], strength: Vector[Int]): Vector[Int] = {
    val s = strength.size - 1
    (for (i <- 0 to s) yield {
      val dominators = getCol(i, dominanceMatrix)
      (for (j <- 0 to s) yield dominators(j) * strength(j)).sum
    }).toVector
  }

  def distance[T](p1: Vector[T], p2: Vector[T])(implicit numeric: Numeric[T]): Double = {
    import numeric._
    if (p1.size != p2.size) throw new GAException("Cannot compare vectors of unequal sizes")
    val s = p1.size- 1
    val sumProduct = (for(i <- 0 to s) yield {
      val d = scala.math.abs(p1(i).toDouble() - p2(i).toDouble())
      d*d
    }).sum
    scala.math.sqrt(sumProduct)
  }

  def distanceMatrix[T](pop: Vector[Vector[T]])(implicit numeric: Numeric[T]): Vector[Vector[Double]] = {
    val s = pop.size - 1
    (for (i <- 0 to s) yield {
      (for (j <- 0 to s) yield {
        val p1 = pop(i)
        val p2 = pop(j)
        distance(p1, p2)
      }).toVector
    }).toVector
  }

  def kthNearestNeighbors(k: Int, distMatrix: Vector[Vector[Double]]): Vector[Double] = {
    if (k >= distMatrix.size ) throw new GAException("Cannot get " + k + "th distance for matrix of size " + distMatrix.size)
    distMatrix.map { row =>
      val sorted = row.sorted
      sorted(k)
    }
  }

  def density(kDistance: Vector[Double]): Vector[Double] = kDistance.map(d => 1.0/(d + 2.0))

  def spea2Fitness(rankVector: Vector[Int], density: Vector[Double]): Vector[Double] = {
    (for (i <- 0 to rankVector.size - 1) yield rankVector(i) + density(i)).toVector
  }
}





