package sGeneticAlgorithm.ga

import sGeneticAlgorithm.ga.GA.{EvaluatedPopulation, ArchiveUpdater}

class BestNArchiveUpdater[T, I <: Iterable[T], F: Ordering](archiveSize: Int, val minimize: Boolean = false) extends ArchiveUpdater[T, I, F] {
  override def updateArchive(oldArchive: EvaluatedPopulation[T, I, F], newPopulation: EvaluatedPopulation[T, I, F]): EvaluatedPopulation[T, I, F] = {
    // remove genomes which are already in the archive
    val filteredPop = newPopulation.filter(g => oldArchive.map(_.genome).contains(g.genome) == false)
    val newArchive: EvaluatedPopulation[T, I, F] = minimize match {
      case false => (oldArchive ++ filteredPop.sortBy(_.fitness).takeRight(archiveSize)).sortBy(_.fitness).takeRight(archiveSize)
      case true =>  (oldArchive ++ filteredPop.sortBy(_.fitness).take(archiveSize)).sortBy(_.fitness).take(archiveSize)
    }

    //println("Old arvhive fitness: " + oldArchive.map(_.fitness))
    //println("New population fitness: " + newPopulation.map(_.fitness))
    //println("New arvhie fitness: " + newArchive.map(_.fitness))
    newArchive
  }
}
