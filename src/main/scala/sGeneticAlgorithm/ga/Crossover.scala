package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom
import GA._


class MultiPointCrossover[T](random: SimRandom, numCrossoverPoints: Int, numChildren: Int) extends Crossover[T, Vector[T]] {
  def crossover(mom: Genome[T, Vector[T]], dad: Genome[T, Vector[T]]): Population[T, Vector[T]] = {
    (for (i <- 1 to numChildren) yield {
      val crossoverPoints = (for (j <- 1 to numCrossoverPoints) yield (random.nextDouble() * (mom.size+1)).toInt).sorted

      // a value is a spouse value if there are an odd number of crossover points before the index
      def spouseValue(offset: Int): Boolean = crossoverPoints.filter(_ <= offset).size % 2 == 1
      (for (i <- 0 to mom.size - 1) yield {
        spouseValue(i) match {
          case true => mom(i)
          case false => dad(i)
        }
      }).toVector
    }).toVector
  }
}

class NoCrossover[T, I <: Iterable[T]] extends Crossover[T, I] {
  def crossover(mom: Genome[T, I], dad: Genome[T, I]): Population[T, I] = {
    Vector(mom, dad)
  }
}