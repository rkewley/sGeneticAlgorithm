package sGeneticAlgorithm.ga

import sGeneticAlgorithm.ga.GA.{EvaluatedPopulation, ArchiveUpdater}

/**
  * Created by ltf on 10/19/16.
  */
class BestNArchiveUpdater[T, I <: Iterable[T], F: Ordering](archiveSize: Int) extends ArchiveUpdater[T, I, F] {
  override def updateArchive(oldArchive: EvaluatedPopulation[T, I, F], newPopulation: EvaluatedPopulation[T, I, F]): EvaluatedPopulation[T, I, F] = {
    val newArchive: EvaluatedPopulation[T, I, F] = (oldArchive ++ newPopulation.sortBy(_.fitness).takeRight(archiveSize)).sortBy(_.fitness).takeRight(archiveSize)
    newArchive
  }
}
