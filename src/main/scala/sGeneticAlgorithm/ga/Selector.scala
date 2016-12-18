package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom

import scala.collection.immutable.TreeMap

import GA._
case class TournamentSelector[T, I <: Iterable[T], F: Ordering](val random: SimRandom, val numInTournamet: Int) extends Selector[T, I, F] {
  def selectFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOptioin: Option[EvaluatedPopulation[T, I, F]] = None): EvaluatedGenome[T, I, F] = {
    val selected = for (i <- 1 to numInTournamet) yield {
      val selectIndex = (random.nextDouble() * evaluated.size).toInt
      evaluated(selectIndex)
    }
    selected.maxBy(_.fitness)
  }
}

class BestSelector[T, I <: Iterable[T], F: Ordering] extends MultiSelector[T, I, F] {
  def selectMultiFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOptioin: Option[EvaluatedPopulation[T, I, F]] = None, n: Int): EvaluatedPopulation[T, I, F] = {
    evaluated.sortBy(_.fitness).takeRight(n)
  }
}

class RandomSelector[T, I <: Iterable[T], F: Ordering](random: SimRandom) extends Selector[T, I, F] {
  override def selectFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): EvaluatedGenome[T, I, F] = {
    evaluated((random.nextDouble() * evaluated.size).toInt)
  }
}