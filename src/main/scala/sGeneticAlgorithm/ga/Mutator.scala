package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom
import GA._


case class AlleleSetMutator[T](val random: SimRandom, val alleleSets: Vector[AlleleSet[_ <: T]], mutationRate: Double) extends Mutator[T, Vector[T]] {
  override def mutate(genome: Genome[T, Vector[T]]): Genome[T, Vector[T]] = {
    (for (i <- 0 to genome.length - 1) yield {
      random.nextDouble() match {
        case x if x < mutationRate => alleleSets(i).getRandomValue
        case _ => genome(i)
      }
    }).toVector
  }
}
