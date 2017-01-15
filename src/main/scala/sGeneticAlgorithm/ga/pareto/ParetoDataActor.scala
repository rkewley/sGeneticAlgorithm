package sGeneticAlgorithm.ga.pareto

import akka.actor.Actor
import sGeneticAlgorithm.ga.GA._

case class GenomeData[T, I <: Seq[T], F: Numeric](val speciesNumber: Int, val popNumber: Int, val genomeNumber: Int, val archive: Boolean,
                                                  genome: Genome[T, I]) {
  def header: String = {
    val fHeader= for (i <- 0 to genome.size - 1) yield s"Value $i"
    "Species, Population, Genome, Archive?, " + fHeader.mkString(", ")
  }
  def values: String = s"$speciesNumber, $popNumber, $genomeNumber, $archive, " + genome.mkString(", ")
}

case class EvaluatedGenomeData[T, I<: Seq[T], F: Numeric](val speciesNumber: Int, val popNumber: Int, val genomeNumber: Int,
                                                          archive: Boolean, eg: EvaluatedGenome[T, I, SPEA2Fitness[F]]) {
  def headerData: String = "Species, Population, Genome, Archive?, "
  def objFitnessHeader: String = {
    {
      val fHeader= for (i <- 0 to eg.fitness.objectiveFitness.size - 1) yield s"Fitness $i"
      headerData + fHeader.mkString(", ")
    }
  }
  def distanceHeader = {
    val dHeader= for (i <- 0 to eg.fitness.sortedDistances.size - 1) yield s"Distance $i"
    headerData + dHeader.mkString(", ")
  }
  def idData: String = s"$speciesNumber, $popNumber, $genomeNumber, $archive, "
  def objFitness: String = idData + eg.fitness.objectiveFitness.mkString(", ")
  def fitnessData: String = idData + spea2Data
  def spea2Data: String = {
    val f = eg.fitness
    s"${f.density}, ${f.rank}, ${f.fitness}"
  }
  def distanceData: String = idData + eg.fitness.sortedDistances.mkString(", ")
}

class ParetoDataActor[T, I <: Iterable[T], F: Numeric] extends Actor {
  var history: Vector[EvaluatedSpeciesArchive[T, I, SPEA2Fitness[Long]]] = Vector()

  def values(sNum: Int, pNum: Int, gNum: Int, genome: Genome[T, I]):
  def receive = {
    case esa: EvaluatedSpeciesArchive[T, I, SPEA2Fitness[Long]] =>
      history = history :+ esa
      printFitnessHistories
      printPopulations
      printArchives
      printArchiveFitness
  }

  def printArchiveFitness: Unit = {
    val currentArchives: Option[Vector[EvaluatedPopulation[T, I, SPEA2Fitness[Long]]]] = history.map(_.archives).last
    currentArchives match {
      case Some(ar) =>
        ar.foreach(ep => println("Archive rank for generation " + history.size + ": " + ep.map(_.fitness.rank)))
        ar.foreach(ep => println("Archive density for generation " + history.size + ": " + ep.map(_.fitness.density)))
        ar.foreach(ep => println("Archive fitness for generation " + history.size + ": " + ep.map(_.fitness.fitness)))
        ar.foreach(ep => println("Archive objective fitness for generation " + history.size + ": " + ep.map(_.fitness.objectiveFitness)))
    }

  }

  def printArchives: Unit = {
    val currentArchives: Option[Vector[EvaluatedPopulation[T, I, SPEA2Fitness[Long]]]] = history.map(_.archives).last
    currentArchives match {
      case Some(ar) =>
        ar.foreach { ep =>
          println("Archive for generation " + history.size + ": ")
          ep.foreach(eg => println(eg.genome.toString()))
        }
    }
  }

  def printPopulations: Unit = {
    val currentPopulations = history.last.populations
    println("Populations for generation " + history.size + ":")
    currentPopulations.foreach { p =>
      p.foreach(eg => println(eg.genome))
      println
    }
  }

  def printFitnessHistories: Unit = {
    val size = history(0).populations.size
    val generation = history.size
    for (i <- 0 to size-1) {
      println("Generation " + generation + " history " + i + "L " + getFitnessHistory(i))
    }
  }

  def getAvgFitness(ep: EvaluatedPopulation[T, I, SPEA2Fitness[Long]]): Double = (ep.map(_.fitness.fitness).sum)/ep.size

  def getPopulationHistory(i: Int): Vector[EvaluatedPopulation[T, I, SPEA2Fitness[Long]]] = {
    val populations = history.map(_.populations)
    populations.map(_(i))
  }

  def getFitnessHistory(i: Int): Vector[Double] = getPopulationHistory(i).map(getAvgFitness(_))


}

