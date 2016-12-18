package sGeneticAlgorithm.ga

import sGeneticAlgorithm.ga.GA._

/**
  * Created by ltf on 10/19/16.
  */
trait GenomeEvaluator[T, I <: Iterable[T], F] {
  def evaluateGenome(g: Genome[T,I]): EvaluatedGenome[T, I, F]
}

trait PopulationEvaluator[T, I <: Iterable[T], F]  {
  def evaluatePopulation(p: Population[T, I]): EvaluatedPopulation[T, I, F]
}

trait SpeciesEvaluator[T, I <: Iterable[T], F] {
  def evaluateSpecies(s: Species[T, I]): EvaluatedSpecies[T, I, F]
}

trait SimpleEvaluator[T, I <: Iterable[T], F] extends Evaluator[T, I, F]
    with SpeciesEvaluator[T, I, F]
    with PopulationEvaluator[T, I, F]
    with GenomeEvaluator[T, I, F] {
  override def evaluate(speciesVector: Vector[Species[T, I]]): Vector[EvaluatedSpecies[T, I, F]] = {
    (for (s <- speciesVector) yield (evaluateSpecies(s))).toVector
  }

  override def evaluateSpecies(s: Species[T, I]): EvaluatedSpecies[T, I, F] = {
    (for (p <- s) yield evaluatePopulation(p)).toVector
  }

  override def evaluatePopulation(p: Population[T, I]): EvaluatedPopulation[T, I, F] = {
    (for (g <- p) yield (evaluateGenome(g)))
  }

}
