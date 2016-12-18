package ga

import org.scalatest.FlatSpec
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom


class MigrateSpec extends FlatSpec {
  val random = new SimRandom(0)

  "A migrater" should "transfer population members to adjacent populations" in {
    // Create a species with 3 populations of 5 identical menmbers each
    val species: Species[Any, Vector[Any]] = (for (i <- 0 to 2) yield {
      (for (j <- 1 to 5) yield Vector(i.toLong)).toVector
    }).toVector

    // Evaluate the population so that fitness is the average of the values
    class FirstEvaluator extends SimpleEvaluator[Any, Vector[Any], Double] {
      override def evaluateGenome(g: Genome[Any, Vector[Any]]): EvaluatedGenome[Any, Vector[Any], Double] = {
        new EvaluatedGenome[Any, Vector[Any], Double](g, g.map(_.asInstanceOf[Long]).sum/g.size)
      }
    }

    val evaluator = new FirstEvaluator

    val evaluatedSpecies: EvaluatedSpecies[Any, Vector[Any], Double] = evaluator.evaluateSpecies(species)

    val selector = new RandomSelector[Any, Vector[Any], Double](random)

    val migrater = new SelectionMigrator[Any, Vector[Any], Double](selector, 0.40)

    val migratedSpecies = migrater.migrate(evaluatedSpecies)

    val zero = migratedSpecies(0).map(_.genome).flatten.map(_.asInstanceOf[Long].toInt).sorted
    val one = migratedSpecies(1).map(_.genome).flatten.map(_.asInstanceOf[Long].toInt).sorted
    val two = migratedSpecies(2).map(_.genome).flatten.map(_.asInstanceOf[Long].toInt).sorted

    assert(zero == Vector(0, 0, 0, 2, 2))
    assert(one == Vector(0, 0, 1, 1, 1))
    assert(two == Vector(1, 1, 2, 2, 2))
  }
}
