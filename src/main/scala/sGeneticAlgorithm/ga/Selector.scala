package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom

import scala.collection.immutable.TreeMap

import GA._
case class TournamentSelector[T, I <: Iterable[T], F: Ordering](val random: SimRandom, val numInTournamet: Int, val minimize: Boolean = false) extends Selector[T, I, F] {
  def selectFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]] = None): EvaluatedGenome[T, I, F] = {
    val selected = for (i <- 1 to numInTournamet) yield {
      val selectIndex = (random.nextDouble() * evaluated.size).toInt
      evaluated(selectIndex)
    }
    minimize match {
      case false => selected.maxBy(_.fitness)
      case true => selected.minBy(_.fitness)
    }
  }
}

class BestSelector[T, I <: Iterable[T], F: Ordering](val minimize: Boolean = false) extends MultiSelector[T, I, F] {
  def selectMultiFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOptioin: Option[EvaluatedPopulation[T, I, F]] = None, n: Int): EvaluatedPopulation[T, I, F] = {
    val sorted = evaluated.sortBy(_.fitness).takeRight(n)
    minimize match {
      case false => sorted.takeRight(n)
      case true => sorted.take(n)
    }
  }
}

class RandomSelector[T, I <: Iterable[T], F: Ordering](random: SimRandom) extends Selector[T, I, F] {
  override def selectFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): EvaluatedGenome[T, I, F] = {
    evaluated((random.nextDouble() * evaluated.size).toInt)
  }
}