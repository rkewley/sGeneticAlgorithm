package sGeneticAlgorithm.ga.pareto

import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import scala.math.Ordering.Implicits._

class OnDiagonalFitnessEvaluator extends GenomeEvaluator[Long, Vector[Long], Vector[Long]] {
  override def evaluateGenome(g: Genome[Long, Vector[Long]]): EvaluatedGenome[Long, Vector[Long], Vector[Long]] = {
    //val dist: Long = 10 - math.abs(g(0) - g(1))
    new EvaluatedGenome[Long, Vector[Long], Vector[Long]](g, Vector(10 - math.min(g(0), g(1)), math.max(g(0), g(1))))
  }
}