package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom
import GA._


case class AllelesInitializer[T](val alleleSets: Vector[AlleleSet[_ <: T]], val populationSize: Int) extends GenomeInitializer[T, Vector[T]] {
  override def initialize: Population[T, Vector[T]] = {
    (for (i <- 1 to populationSize) yield alleleSets.map(_.getRandomValue)).toVector
  }
}
