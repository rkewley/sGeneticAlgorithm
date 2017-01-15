package sGeneticAlgorithm.ga.pareto

import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.ga.GA._

import scala.annotation.tailrec

/**
  * Created by dmf on 1/2/17.
  */
class SPEA2Evolver[T, I <: Iterable[T], F: Ordering](val mutator: Mutator[T, I],
                                                     val crosser: Crossover[T, I],
                                                     val parentSelector: Selector[T, I, F],
                                                     val archiveUpdater: ArchiveUpdater[T, I, F],
                                                     archiveSize: Int) extends GA.Evolver[T, I, F] {

  def mutate(genome: Genome[T, I]) = mutator.mutate(genome)
  def crossover(mom: Genome[T, I], dad: Genome[T, I]) = crosser.crossover(mom, dad)
  def selectParent(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]) = parentSelector.selectFrom(evaluated, archiveOption)

  @tailrec
  final def addChildren(population: Population[T, I], evaluatedPopulation: EvaluatedPopulation[T, I, F], totalChildren: Int, archiveOption: Option[EvaluatedPopulation[T, I, F]] = None): Population[T, I] = {
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
    if (archiveOption == None) throw new GAException("A SPEA2 algorithm requires an archive")
    val upddatedArchive = archiveOption.map(ar => archiveUpdater.updateArchive(ar, evaluatedPopulation))

    // Evolve children for next generation
    val newChildren = addChildren(Vector[Genome[T, I]](), upddatedArchive.get, archiveSize)
    new PopArchive(newChildren, upddatedArchive)
  }
}
