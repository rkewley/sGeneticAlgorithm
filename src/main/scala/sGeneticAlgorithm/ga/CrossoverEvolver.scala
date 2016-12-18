package sGeneticAlgorithm.ga

import GA._

import scala.annotation.tailrec

class CrossoverEvolver[T, I <: Iterable[T], F: Ordering](val mutator: Mutator[T, I],
                                                         val crosser: Crossover[T, I],
                                                         val parentSelector: Selector[T, I, F],
                                                         val survivorSelector: MultiSelector[T, I, F],
                                                         val archiveUpdater: ArchiveUpdater[T, I, F],
                                                         val replacementRate: Double) extends GA.Evolver[T, I, F] {

  def mutate(genome: Genome[T, I]) = mutator.mutate(genome)
  def crossover(mom: Genome[T, I], dad: Genome[T, I]) = crosser.crossover(mom, dad)
  def selectParent(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]) = parentSelector.selectFrom(evaluated, archiveOption)
  def selectSurvivors(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]], n: Int) = survivorSelector.selectMultiFrom(evaluated, archiveOption, n)

  @tailrec
  private def addChildren(population: Population[T, I], evaluatedPopulation: EvaluatedPopulation[T, I, F], totalChildren: Int, archiveOption: Option[EvaluatedPopulation[T, I, F]] = None): Population[T, I] = {
    val populationSize = evaluatedPopulation.size
    population.size match {
      case x if x >= totalChildren => population.take(totalChildren)
      case _ =>
        val mom = selectParent(evaluatedPopulation, archiveOption).genome
        val dad = selectParent(evaluatedPopulation, archiveOption).genome
        val children = crossover(mom,dad).map(mutate(_))
        addChildren(population ++ children, evaluatedPopulation, totalChildren)
    }
  }

  override def evolve(evaluatedPopulation: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): PopArchive[T, I, F] = {
    if (replacementRate > 1.0 || replacementRate < 0.0) throw new GAException("Replacement rate of " + replacementRate + " is not between 0 and 1")

    val populationSize = evaluatedPopulation.size
    // Evolve children for next generation
    val numNewChildren = (replacementRate * populationSize).toInt
    val newChildren = addChildren(Vector[Genome[T, I]](), evaluatedPopulation, numNewChildren)

    // Add survivors
    val numSurvivors = populationSize - numNewChildren
    val survivors = selectSurvivors(evaluatedPopulation, archiveOption, numSurvivors).map(_.genome)
    val upddatedArchive = archiveOption.map(ar => archiveUpdater.updateArchive(evaluatedPopulation, ar))
    new PopArchive(newChildren ++ survivors, upddatedArchive)
  }

}

