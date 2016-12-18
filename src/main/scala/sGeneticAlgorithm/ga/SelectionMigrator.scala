package sGeneticAlgorithm.ga

import sGeneticAlgorithm.ga.GA._

/**
  * Created by ltf on 10/19/16.
  */
class SelectionMigrator[T, I <: Iterable[T], F: Ordering](val selector: Selector[T, I, F], val migrationRate: Double) extends Migrater[T, I, F] {

  override def migrate(evaluatedSpecies: EvaluatedSpecies[T, I, F]): EvaluatedSpecies[T, I, F] = {
    val numMigrants = (evaluatedSpecies(0).size * migrationRate).toInt
    val movers = (for (es <- evaluatedSpecies) yield selector.selectMultiFrom(es, None, numMigrants)).toVector
    val newSpecies = for (i <- 0 to evaluatedSpecies.size -1) yield {
      val newComers = i match {
        case x if x == 0 => movers(evaluatedSpecies.size - 1)
        case _ => movers(i - 1)
      }
      evaluatedSpecies(i).diff(movers(i)) ++ newComers
    }
    newSpecies.toVector
  }
}
