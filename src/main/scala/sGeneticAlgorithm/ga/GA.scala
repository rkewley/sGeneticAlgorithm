package sGeneticAlgorithm.ga
import akka.actor._


object GA {
  case class Unique[T]( val any: Any, val uuid: String = java.util.UUID.randomUUID.toString) extends Serializable
  case object Start
  type Genome[T, I <: Iterable[T]] = I
  type Population[T, I <: Iterable[T]] = Vector[Genome[T, I]]
  type Species[T, I <: Iterable[T]] = Vector[Population[T, I]]
  type EvaluatedSpecies[T, I <: Iterable[T], F] = Vector[EvaluatedPopulation[T, I, F]]
  type EvaluatedPopulation[T, I <: Iterable[T], F] = Vector[EvaluatedGenome[T, I, F]]
  type Ecosystem = Vector[Species[_, _ <: Iterable[_]]]
  type EvaluatedEcosystem = Vector[EvaluatedSpecies[_, _ <: Iterable[_], _]]

  class EvaluatedGenome[T, I <: Iterable[T], F: Ordering](val genome: Genome[T, I], val fitness: F)
  case class PopArchive[T, I <: Iterable[T], F: Ordering](pop: Population[T, I], archiveOption: Option[EvaluatedPopulation[T,I,F]] = None)
  case class EvaluatedPopArchive[T, I <: Iterable[T],F: Ordering](evaluatedPop: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T,I,F]])
  case class SpeciesArchive[T, I <: Iterable[T], F: Ordering](popArchives: Vector[PopArchive[T, I, F]]) {
    def populations: Species[T, I] = {
      popArchives.map(_.pop)
    }
    def archives: Option[Vector[EvaluatedPopulation[T, I, F]]] = {
      val options: Vector[Option[EvaluatedPopulation[T, I, F]]] = popArchives.map(_.archiveOption)
      options.isEmpty match {
        case true => None
        case false => Some(options.map(_.get))
      }
    }
  }
  case class EvaluatedSpeciesArchive[T,I <: Iterable[T], F: Ordering](evaluatedPopArchives: Vector[EvaluatedPopArchive[T, I, F]]) {
    def this(evSpecies: EvaluatedSpecies[T, I, F], archives: Option[Vector[EvaluatedPopulation[T, I, F]]]) = {
      this((for (i <- 0 to evSpecies.size - 1) yield EvaluatedPopArchive(evSpecies(i), archives.map(_(i)))).toVector)
    }
    def populations: EvaluatedSpecies[T, I, F] = {
      evaluatedPopArchives.map(_.evaluatedPop)
    }
    def archives: Option[Vector[EvaluatedPopulation[T, I, F]]] = {
      val options: Vector[Option[EvaluatedPopulation[T, I, F]]] = evaluatedPopArchives.map(_.archiveOption)
      options.isEmpty match {
        case true => None
        case false => Some(options.map(_.get))
      }
    }
  }

  case class EcosystemArchive(speciesArchives: Vector[SpeciesArchive[_, _ <: Iterable[_], _]])
  case class EvaluatedEcosysteemArchive(evaluatedSpeciesArchives: Vector[EvaluatedSpeciesArchive[_, _ <: Iterable[_], _]])


  trait GenomeInitializer[T, I <: Iterable[T]] {
    def initialize: Population[T, I]
  }

  trait Mutator[T, I <: Iterable[T]] {
    def mutate(genome: Genome[T, I]): Genome[T, I]
  }

  trait Crossover[T, I <: Iterable[T]] {
    def crossover(mom: Genome[T, I], dad: Genome[T, I]): Population[T, I]
  }

  abstract class Evaluator[T, I <: Iterable[T], F: Ordering] {
    def evaluate(speciesVector: Vector[Species[T, I]]): Vector[EvaluatedSpecies[T, I, F]]
  }

  abstract class Selector[T, I <: Iterable[T], F: Ordering] extends MultiSelector[T, I, F]{
    def selectFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): EvaluatedGenome[T, I, F]
    private def selectNext(n: Int, evaluated: EvaluatedPopulation[T, I, F], selections: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): EvaluatedPopulation[T, I, F] = {
      selections.size == n match {
        case true => selections
        case false =>
          val selected = selectFrom(evaluated, archiveOption)
          selectNext(n, evaluated.diff(Vector(selected)), selections.+:(selected), archiveOption)
      }
    }
    def selectMultiFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]], numToSelect: Int): EvaluatedPopulation[T, I, F] = {
      selectNext(numToSelect, evaluated, Vector[EvaluatedGenome[T, I, F]](), archiveOption)
    }
  }

  abstract class MultiSelector[T, I <: Iterable[T], F: Ordering] {
    def selectMultiFrom(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]], n: Int): EvaluatedPopulation[T, I, F]
  }

  abstract class Migrater[T, I <: Iterable[T], F: Ordering] {
    def migrate(evaluatedSpecies: EvaluatedSpecies[T, I, F]): EvaluatedSpecies[T, I, F]
  }

  abstract class Evolver[T, I <: Iterable[T], F: Ordering] {
    def evolve(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): PopArchive[T, I, F]
  }

  abstract class ArchiveUpdater[T, I <: Iterable[T], F: Ordering] {
    def updateArchive(oldArchive: EvaluatedPopulation[T, I, F], newPopulation: EvaluatedPopulation[T, I, F]): EvaluatedPopulation[T, I, F]
  }

  abstract class StopCondition[T, I <: Iterable[T], F: Ordering] {
    def checkStop(thisGeneration: Vector[SpeciesArchive[T, I, F]], nextGeneration: Vector[SpeciesArchive[T, I, F]]): Boolean
  }

}

import GA._

class GAException(message: String) extends Exception(message)

class GA[T, I <: Iterable[T], F: Ordering](val evaluatorActor: ActorRef,
                                           val dataActor: ActorRef,
                                           val migrater: Migrater[T, I, F],
                                           val evolver: Evolver[T, I, F],
                                           firstGeneration: SpeciesArchive[T, I, F],
                                           numGenerations: Int) extends Actor {

  var generation: Int = 0
  def migrate(evaluatedSpecies: EvaluatedSpecies[T, I, F]) = migrater.migrate(evaluatedSpecies)
  def evolvePopulation(evaluated: EvaluatedPopulation[T, I, F], archiveOption: Option[EvaluatedPopulation[T, I, F]]): PopArchive[T, I, F] = {
    evolver.evolve(evaluated, archiveOption)
  }


  def receive = {
    case esa: EvaluatedSpeciesArchive[T, I, F] =>
      // log the species evaluation to the data actor
      dataActor ! esa

      // continue to evolve if we have not reached the prescribed number of generations
      generation = generation + 1
      if (generation < numGenerations) {
        val nextGeneration: SpeciesArchive[T, I, F] = evolveGeneration(esa)
        evaluatorActor ! nextGeneration
      }

    case Start =>
      evaluatorActor ! firstGeneration
  }

  def evolveGeneration(evaluatedSpeciesArchive: EvaluatedSpeciesArchive[T,I,F]): SpeciesArchive[T, I, F] = {
      val migratedSpecies: EvaluatedSpecies[T, I, F] = migrate(evaluatedSpeciesArchive.populations)
      val archiveVectorOption = evaluatedSpeciesArchive.archives
      val speciesArchive: SpeciesArchive[T,I,F] = SpeciesArchive((for (j <- 0 to migratedSpecies.size - 1) yield {
        val evaluatedPopulationn: EvaluatedPopulation[T,I,F]  = migratedSpecies(j)
        val archive: Option[EvaluatedPopulation[T,I,F]] = archiveVectorOption.map {av => av(j)}
        evolvePopulation(evaluatedPopulationn, archive)
      }).toVector)
      speciesArchive
  }

}
