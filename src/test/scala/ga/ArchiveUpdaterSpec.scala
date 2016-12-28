package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom

class ArchiveUpdaterSpec extends FlatSpec {
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

  "An ArchiveUpdater" should "add the best elements to the population" in {
    val archive1: Population[Long, Vector[Long]] = (for (i <- 101 to 110) yield {
      (for (j <- 1 to 100) yield i.toLong).toVector
    }).toVector

    val archive2: Population[Long, Vector[Long]] = (for (i <- 1 to 10) yield {
      (for (j <- 1 to 100) yield i.toLong).toVector
    }).toVector

    val archive3: Population[Long, Vector[Long]] = archive1 ++ archive2

    val archive: EvaluatedPopulation[Long, Vector[Long], Double] = new FirstEvaluator().evaluatePopulation(archive3)

    val evaluated: EvaluatedPopulation[Long, Vector[Long], Double] = new FirstEvaluator().evaluatePopulation(pop)

    val archiveUpdater = new BestNArchiveUpdater[Long, Vector[Long], Double](20)

    val newArchive = archiveUpdater.updateArchive(archive, evaluated)

    // New population should have elements from 91-110
    val newValues = newArchive.map(ev => ev.genome(0).asInstanceOf[Long]).sorted

    val test = (for (i <- 91 to 110) yield i.toLong).toVector.sorted

    assert(newValues == test)


  }

}
