package ga

import org.scalatest._
import sGeneticAlgorithm.ga.pareto.ParetoFitness._
import sGeneticAlgorithm.ga._

import scala.math.Ordering._


class ParetoFitnessTest extends FlatSpec {
  val d1 = Vector[Double](1.0, 2.0, 3.0)
  val d2 = Vector[Double](1.2, 2.2, 3.2)
  val d3 = Vector[Double](1.0, 2.1, 3.1)
  val d4 = Vector[Double](1.1, 2.1, 3.1)
  val d5 = Vector[Double](1.1, 1.9, 3.1)
  val d6 = Vector[Double](1.1, 2.1, 2.9)

  "A ParetoFitness" should "correctly determine dominace" in {



    val dom2 = dominates(d2, d1)
    assert(dom2 == true)

    val dom3 = dominates(d3, d1)
    assert(dom3 == true)

    val dom4 = dominates(d4, d1)
    assert(dom4 == true)

    val dom5 = dominates(d5, d1)
    assert(dom5 == false)

    val dom6 = dominates(d6, d1)
    assert(dom6 == false)

    val l1 = Vector[Long](1, 2, 3)
    val l2 = Vector[Long](4, 5, 6)
    val l3 = Vector[Long](1, 5, 6)
    val l4 = Vector[Long](0, 5, 6)
    val l5 = Vector[Long](4, 1, 6)
    val l6 = Vector[Long](4, 5, 2)

    val doml2 = dominates(l2, l1)
    assert(doml2 == true)

    val doml3 = dominates(l3, l1)
    assert(doml3 == true)

    val doml4 = dominates(l4, l1)
    assert(doml4 == false)

    val doml5 = dominates(l5, l1)
    assert(doml5 == false)

    val doml6 = dominates(l6, l1)
    assert(doml6 == false)
  }

  "A ParetoFitness" should "correctly calculate dominance matrix" in {
    val fitVectors = Vector(d1, d2, d3, d4)
    val dMatrix = dominanceMatrix(fitVectors)
    assert(dMatrix(0) == Vector(0, 0, 0, 0))
    assert(dMatrix(1) == Vector(1, 0, 1, 1))
    assert(dMatrix(2) == Vector(1, 0, 0, 0))
    assert(dMatrix(3) == Vector(1, 0, 1, 0))

  }


  "A ParetoFitness" should "correctly calculate strength" in {
    val fitVectors = Vector(d1, d2, d3, d4)
    val dMatrix = dominanceMatrix(fitVectors)

    val strengths = strengthFromMatrix(dMatrix)
    assert(strengths == Vector(0, 3, 1, 2))

  }

  "A ParetoFitness" should "correctly calculate ranks" in {
    val fitVectors = Vector(d1, d2, d3, d4)
    val dMatrix = dominanceMatrix(fitVectors)

    val strengths = strengthFromMatrix(dMatrix)

    val rankVector: Vector[Int] = ranks(dMatrix, strengths)
    assert(rankVector == Vector(6, 0, 5, 3))

  }

  "A ParetoArchiveUpdater" should "correctly calculate distance and density" in {
    val p1 = Vector[Int](1, 2, 3)
    val p2 = Vector[Int](3, 12, 14)
    val p3 = Vector[Int](5, 15, 19)
    val d = distance(p1, p2)
    assert(distance(p1, p2) == 15.0)

    val distMatrix = distanceMatrix(Vector[Vector[Int]](p1, p2, p3))
    assert(distMatrix(0) == Vector(0, 15, 21))
    assert(distMatrix(1)(0) == 15)
    assert(distMatrix(2)(0) == 21)

    val k1Distances = kthNearestNeighbors(1, distMatrix)
    val k2Distances = kthNearestNeighbors(2, distMatrix )
    assert(k1Distances(0) == 15)
    assert(k1Distances(1) > 6.1 && k1Distances(1) < 6.2)
    assert(k1Distances(2) > 6.1 && k1Distances(2) < 6.2)
    assert(k2Distances(0) == 21)
    assert(k2Distances(1) == 15)
    assert(k2Distances(2) == 21)

    val densities = density(k1Distances)
    densities.foreach(d => assert(d > 0 && d < 1))
    assert(densities(0) < densities(1))
    assert(densities(0) < densities(2))

  }


  "A ParetoFitness" should "correctly get columns of a 2d matric" in {
    val fitVectors = Vector(d1, d2, d3, d4)
    val dMatrix = dominanceMatrix(fitVectors)
    assert(dMatrix(0) == Vector(0, 0, 0, 0))
    assert(dMatrix(1) == Vector(1, 0, 1, 1))
    assert(dMatrix(2) == Vector(1, 0, 0, 0))
    assert(dMatrix(3) == Vector(1, 0, 1, 0))
    assert(getCol(0, dMatrix) == Vector(0, 1, 1, 1))
    assert(getCol(1, dMatrix) == Vector(0, 0, 0, 0))
    assert(getCol(2, dMatrix) == Vector(0, 1, 0, 1))
    assert(getCol(3, dMatrix) == Vector(0, 1, 0, 0))


  }


  "A ParetoFitness" should "correctly calculate fitness" in {
    val fitVectors = Vector(d1, d2, d3, d4)
    val dMatrix = dominanceMatrix(fitVectors)

    val strengths = strengthFromMatrix(dMatrix)

    val rankVector: Vector[Int] = ranks(dMatrix, strengths)
    assert(rankVector == Vector(6, 0, 5, 3))

    val distMatrix = distanceMatrix(fitVectors)
    val k1Distances = kthNearestNeighbors(1, distMatrix)
    val densityVector = density(k1Distances)
    val fitVector = spea2Fitness(rankVector, densityVector)
    assert(fitVector(0) > 6.466 && fitVector(0) < 6.467)
    assert(fitVector(1) > 0.46 && fitVector(1) < 0.47)
  }

}
