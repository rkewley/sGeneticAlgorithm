package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom

class EvolverSpec extends FlatSpec {
  val random = new SimRandom(0)
  //Create a population with uniform genomes
  val pop: Population[Long, Vector[Long]] = (for (i <- 1 to 100) yield {
    (for (j <- 1 to 100) yield i.toLong).toVector
  }).toVector

  // Evaluate the population so that fitness is the average of the values
  class FirstEvaluator extends SimpleEvaluator[Long, Vector[Long], Double] {
    override def evaluateGenome(g: Genome[Long, Vector[Long]]): EvaluatedGenome[Long, Vector[Long], Double] = {
      new EvaluatedGenome[Long, Vector[Long], Double](g, g.sum/g.size)
    }
  }

  val species = Vector(pop)
  val animals = Vector(species)
  val evaluator = new FirstEvaluator
  val evaluatedSpecies = evaluator.evaluate(animals)
  val evaluatedPopulation = evaluatedSpecies(0)(0)

  "An Evolver" should "evolve the population with crossover only" in {

    // First try with no mutation
    val numCrossoverPoints = 2
    val numChildren = 1
    val mutationRate = 0.0
    val numInTournament = 2
    val replacementRate = 0.5

    val crossover = new MultiPointCrossover[Long](random, numCrossoverPoints, numChildren)
    val ias = LongAlleleSet(random, 101, 200)
    val alleles = (for (i <- 1 to 100) yield ias).toVector
    val mutator = AlleleSetMutator[Long](random, alleles, mutationRate)
    val tournamentSelector = new TournamentSelector[Long, Vector[Long], Double](random, numInTournament)
    val bestSelector = new BestSelector[Long, Vector[Long], Double]
    val archiveUpdater = new BestNArchiveUpdater[Long, Vector[Long], Double](20)
    val evolver = new CrossoverEvolver(mutator, crossover, tournamentSelector, bestSelector, archiveUpdater, replacementRate)
    val nextGeneration = evolver.evolve(evaluatedPopulation, None).pop

    // The new population should be the same size as the old
    assert(nextGeneration.size == evaluatedPopulation.size)

    // About half of the new population from crossover should have two unique values
    val distinctValues = nextGeneration.map(_.distinct.size)
    val twos = distinctValues.filter(v => v == 2)
    val ones = distinctValues.filter(v => v == 1)
    assert(twos.size > 45)
    // The survivors should have one unique value
    assert(ones.size >= 50)

    // The average fitness should increase
    val nextEvaluated = evaluator.evaluatePopulation(nextGeneration)
    val numbers = nextEvaluated.map(_.fitness)
    val average = numbers.sum/numbers.size
    assert(average > 60)
  }

  "An Evolver" should "evolve the population with mutation only" in {

    // First try with no mutation

    val mutationRate = 0.1
    val replacementRate = 0.5
    val numInTournament = 2

    val crossover = new NoCrossover[Long, Vector[Long]]
    val ias = LongAlleleSet(random, 101, 200)
    val alleles = (for (i <- 1 to 100) yield ias).toVector
    val mutator = AlleleSetMutator[Long](random, alleles, mutationRate)
    val tournamentSelector = new TournamentSelector[Long, Vector[Long], Double](random, numInTournament)
    val bestSelector = new BestSelector[Long, Vector[Long], Double]
    val archiveUpdater = new BestNArchiveUpdater[Long, Vector[Long], Double](20)
    val evolver = new CrossoverEvolver(mutator, crossover, tournamentSelector, bestSelector, archiveUpdater, replacementRate)
    val nextGeneration = evolver.evolve(evaluatedPopulation, None).pop

    // The new population should be the same size as the old
    assert(nextGeneration.size == evaluatedPopulation.size)

    // About half of the new population from crossover should have only one value
    val distinctValues = nextGeneration.map(_.distinct.size)
    val more = distinctValues.filter(v => v >= 2)
    val ones = distinctValues.filter(v => v == 1)
    assert(more.size > 45)

    // The survivors should have one unique value
    assert(ones.size >= 50)

    // The average fitness should increase
    val nextEvaluated = evaluator.evaluatePopulation(nextGeneration)
    val numbers = nextEvaluated.map(_.fitness)
    val average = numbers.sum/numbers.size
    assert(average > 60)


  }
}
