package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA.Genome
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom

class CrossoverSpec extends FlatSpec {
  val random = new SimRandom(0)
  val mom: Genome[Long, Vector[Long]] = Vector(1, 1, 1, 1, 1, 1)
  val dad: Genome[Long, Vector[Long]] = Vector(2, 2, 2, 2, 2, 2)
  "An multi-point crossover" should "generate children with crossed values" in {
    val numChildren = 6
    val numCrossoverPoints = 2

    val crosser: MultiPointCrossover[Long] = new MultiPointCrossover(random, numCrossoverPoints, numChildren)

    val children = crosser.crossover(mom, dad)
    println("Genomes with 1's in the middle are: ")
    children.foreach(println)
    assert(children.size == numChildren)
    val crossed = children.map(_.distinct.size).filter(s => s == 2)
    assert(crossed.size > 0)
  }

  "A NoCrossover" should "return mom and dad genomes" in {
    val crosser = new NoCrossover[Long, Vector[Long]]
    val children = crosser.crossover(mom, dad)
    assert(children.size == 2)
    assert(children(0) == Vector(1, 1, 1, 1, 1, 1))
    assert(children(1) == Vector(2, 2, 2, 2, 2, 2))
  }

}
