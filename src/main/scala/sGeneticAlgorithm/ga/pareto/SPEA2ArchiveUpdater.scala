package sGeneticAlgorithm.ga.pareto

import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga.GAException
import scala.math.Numeric._
import scala.annotation.tailrec

class SPEA2FitnessOrdering[FV] extends Ordering[SPEA2Fitness[FV]] {
  override def compare(x: SPEA2Fitness[FV], y: SPEA2Fitness[FV]) = x.fitness.compare(y.fitness)
}
case class SPEA2Fitness[T](val objectiveFitness: Vector[T], val rank: Int, val density: Double, val sortedDistances: Vector[Double], val fitness: Double)
  extends Ordered[SPEA2Fitness[T]] {
  override def compare(that: SPEA2Fitness[T]): Int = this.fitness.compare(that.fitness)
}

trait SPEA2DistanceOrdering[T, I <: Iterable[T], FV] extends Ordering[EvaluatedGenome[T, I, SPEA2Fitness[FV]]] {
  override def compare(x: EvaluatedGenome[T, I, SPEA2Fitness[FV]], y: EvaluatedGenome[T, I, SPEA2Fitness[FV]]) = {
    def comparek(k: Int): Int = {
      x.fitness.sortedDistances(k-1).compare(y.fitness.sortedDistances(k-1)) match {
        case v if v == 0 =>
          k match {
            case v2 if v2 >= x.fitness.sortedDistances.size => v
            case _ => comparek(k + 1)
          }
        case v => v
      }
    }
    comparek(1)
  }
}

class SPEA2ArchiveUpdater[T, I <: Seq[T], FV](archiveSize: Int,
                                                            val minimize: Boolean = true,
                                                            implicit val ord: Ordering[SPEA2Fitness[FV]] = new SPEA2FitnessOrdering[FV])
  extends ArchiveUpdater[T, I, SPEA2Fitness[FV]] with SPEA2DistanceOrdering[T, I, FV]{
  def lowestDistance(candidates: EvaluatedPopulation[T, I, SPEA2Fitness[FV]]): EvaluatedGenome[T, I, SPEA2Fitness[FV]] = {
    @tailrec
    def lowestKthDistance(k: Int, reducedCandidates: EvaluatedPopulation[T, I, SPEA2Fitness[FV]]): EvaluatedGenome[T, I, SPEA2Fitness[FV]] = {
      val minDistance = reducedCandidates.map(_.fitness.sortedDistances(k - 1)).min
      val newCandidates = reducedCandidates.filter(_.fitness.sortedDistances(k - 1) == minDistance)
      newCandidates.size match {
        case x if x == 0 => throw new GAException("Must have at least one member with min " + k + "th distance of " + minDistance)
        case x if x == 1 => newCandidates(0)
        case _ => k match {
          case x if x >= newCandidates.size => newCandidates(0)
          case _ => lowestKthDistance(k + 1, newCandidates)
        }

      }
    }

    lowestKthDistance(1, candidates)
  }

  def removeOne(tooBigArchive: EvaluatedPopulation[T, I, SPEA2Fitness[FV]]): EvaluatedPopulation[T, I, SPEA2Fitness[FV]] = {
    tooBigArchive.size > archiveSize match {
      case false => tooBigArchive
      case true =>
        val itemToRemove: EvaluatedGenome[T, I, SPEA2Fitness[FV]] = lowestDistance(tooBigArchive)
        println("Removing "+itemToRemove.genome(0)+", "+itemToRemove.genome(1)+" with rank "+ itemToRemove.fitness.rank+" and distances "+itemToRemove.fitness.sortedDistances)
        val removeIndex = tooBigArchive.indexOf(itemToRemove)
        //val smallerArchive: EvaluatedPopulation[T, I, SPEA2Fitness[FV]] =
        //  new EvaluatedPopulation[T, I, SPEA2Fitness[FV]]((for (i <- 0 until tooBigArchive.size if i != itemToRemove) yield tooBigArchive(i)).toVector)
        val smallerArchive: Vector[EvaluatedGenome[T, I, SPEA2Fitness[FV]]] = tooBigArchive.take(removeIndex) ++ tooBigArchive.drop(removeIndex + 1)
        removeOne(smallerArchive)
    }
  }


  override def updateArchive(oldArchive: EvaluatedPopulation[T, I, SPEA2Fitness[FV]],
                             newPopulation: EvaluatedPopulation[T, I, SPEA2Fitness[FV]]):
  EvaluatedPopulation[T, I, SPEA2Fitness[FV]] = {
    // add non-dominated solutioins in archive and population to archive
    val nonDominated = (oldArchive ++ newPopulation).filter(g => g.fitness.rank == 0)
    val newArchive =  archiveSize - nonDominated.size match {
      case difference if difference > 0 =>
        // add most fit non-dominant members to the population
        val dominated = (oldArchive ++ newPopulation).filter(g => g.fitness.rank > 0)
        val addedMembers = dominated.sortBy(g => g.fitness).take(difference)
        nonDominated ++ addedMembers
      case difference if difference < 0 =>
        //dominators.sorted(this).take(archiveSize)
        removeOne(nonDominated)
      case difference if difference == 0 => nonDominated
    }
    //println("Old arvhive fitness: " + oldArchive.map(_.fitness))
    //println("New population fitness: " + newPopulation.map(_.fitness))
    //println("New arvhie fitness: " + newArchive.map(_.fitness))
    newArchive
  }
}
