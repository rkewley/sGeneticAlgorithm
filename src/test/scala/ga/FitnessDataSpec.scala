package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom

class FitnessDataSpec extends FlatSpec {
  val random = new SimRandom(0)
  val a: Genome[Long, Vector[Long]] = Vector[Long](1, 1, 1, 1, 1, 1)
  val b: Genome[Long, Vector[Long]] = Vector[Long](2, 2, 2, 2, 2, 2)
  val c: Genome[Long, Vector[Long]] = Vector[Long](3, 3, 3, 3, 3, 3)

  val fit = Vector(b, c, a).map { g =>
    new EvaluatedGenome[Long, Vector[Long], Double](g, g(0).toDouble)
  }

  val selectPopulation: EvaluatedPopulation[Long, Vector[Long], Double] = fit.toVector

  "A Fitness Data object" should "sort by fitness values" in {

    val fits = fit.sortBy(_.fitness)
    assert(fits {
      0
    }.genome(1) == 1)
    assert(fits {
      1
    }.genome(1) == 2)
    assert(fits {
      2
    }.genome(1) == 3)
  }

  "A TournameSelector" should "select more genomes with highter fitness" in {

    val selector = new TournamentSelector[Long, Vector[Long], Double](random, 2)

    val selections = for (i <- 1 to 100) yield selector.selectFrom(selectPopulation).genome
    val ones = selections.filter(g => g(2) == 1)
    val twos = selections.filter(g => g(2) == 2)
    val threes = selections.filter(g => g(2) == 3)
    println("Selected " + ones.size + " ones, " +  twos.size +  " twos, " + threes.size + " threes")
    assert(threes.size > twos.size)
    assert(twos.size > ones.size)
  }

  " A BestSelector" should "select the best genomes" in {
    val selector = new BestSelector[Long, Vector[Long], Double]
    val selections = selector.selectMultiFrom(selectPopulation, None, 2).map(_.genome)
    assert(selections.size == 2)
    assert(selections.map(_(0)).contains(3.toLong))
    assert(selections.map(_(0)).contains(2.toLong))
  }

}
