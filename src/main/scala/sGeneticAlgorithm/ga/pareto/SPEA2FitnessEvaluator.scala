package sGeneticAlgorithm.ga.pareto

import akka.actor.{Actor, ActorRef}
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.ga.GA._

import scala.math.Ordering.Implicits._
import scala.math.Numeric._


class SPEA2FitnessEvaluator[T, S <: Seq[T], FV: Numeric] {
  implicit val ord: Ordering[SPEA2Fitness[FV]] = new SPEA2FitnessOrdering[FV]()
  def evaluatePopArchive(popArchive: EvaluatedPopArchive[T, S, Vector[FV]]): EvaluatedPopArchive[T, S, SPEA2Fitness[FV]] = {
    val p: EvaluatedPopulation[T, S, Vector[FV]] = popArchive.evaluatedPop ++ popArchive.archiveOption.getOrElse(
      throw new GAException("A population archive for the SPEA2 algorithm must have an archive")
    )
    val domMatrix = ParetoFitness.dominanceMatrix(p.map(_.fitness))
    val strengths = ParetoFitness.strengthFromMatrix(domMatrix)
    val ranks = ParetoFitness.ranks(domMatrix, strengths)
    val distMatrix = ParetoFitness.distanceMatrix(p.map(_.fitness))
    val k: Int = scala.math.sqrt(p.size).toInt
    val kDistances = ParetoFitness.kthNearestNeighbors(k, distMatrix)
    val densities = ParetoFitness.density(kDistances)
    val fitnessVector = ParetoFitness.spea2Fitness(ranks, densities)
    val population: EvaluatedPopulation[T, S, SPEA2Fitness[FV]] = (for (i <- 0 to popArchive.evaluatedPop.size -1) yield {
      val g: Genome[T, S] = p(i).genome
      val fv: Vector[FV] = p(i).fitness
      val sortedDistances = distMatrix(i).sorted
      new EvaluatedGenome[T, S, SPEA2Fitness[FV]](g, SPEA2Fitness[FV](fv, ranks(i), densities(i), sortedDistances, fitnessVector(i)))
    }).toVector
    val archive: EvaluatedPopulation[T, S, SPEA2Fitness[FV]] = (for (i <- popArchive.evaluatedPop.size to p.size - 1) yield {
      val g: Genome[T, S] = p(i).genome
      val fv: Vector[FV] = p(i).fitness
      val sortedDistances = distMatrix(i).sorted
      new EvaluatedGenome[T, S, SPEA2Fitness[FV]](g, SPEA2Fitness[FV](fv, ranks(i), densities(i), sortedDistances, fitnessVector(i)))
    }).toVector
    new EvaluatedPopArchive[T, S, SPEA2Fitness[FV]](population, Some(archive))
  }
}
/*
class SPEA2PopulationEvaluationActor[T, S <: Seq[T], FV: Numeric](override val evaluators: List[ActorRef])
  extends PopulationEvaluationActor[T, S, Vector[FV]](evaluators) {

  val spea2FitnessEvaluator = new SPEA2FitnessEvaluator[T, S, FV]

  override def evaluationDone: Unit = {
    val speaPopulation = spea2FitnessEvaluator.evaluatePopulation(evaluatedPopulation)
    popSender ! speaPopulation
  }
}*/

class SPEA2PopulationEvaluationActor[T, I <: Seq[T], FV: Numeric](val evaluators: List[ActorRef]) extends Actor {
  val spea2FitnessEvaluator = new SPEA2FitnessEvaluator[T, I, FV]
  var evaluatedPopulation: Vector[EvaluatedGenome[T, I, Vector[FV]]] = null
  var currentArchive: Vector[EvaluatedGenome[T, I, Vector[FV]]] = null
  var evalIterator = evaluators.iterator
  var popSize = 0
  var popSender: ActorRef = null
  def receive = {
    case popArchive: PopArchive[T, I, SPEA2Fitness[FV]] =>
      //println("Received population for evaluation")
      evaluatedPopulation = Vector[EvaluatedGenome[T, I, Vector[FV]]]()
      currentArchive = popArchive.archiveOption match {
        case None => throw new GAException("SPEA2 populations must have an archive")
          // Throw away all fitness data in the archive except the vector of objective values.  All SPEA2 fitness data must be recalculated
        case Some(ar) => ar.map(eg => new EvaluatedGenome[T, I, Vector[FV]](eg.genome, eg.fitness.objectiveFitness))
      }
      popSize = popArchive.pop.size
      popSender = sender
      popArchive.pop.foreach { genome =>
        evaluate(genome)
      }
    case eg: EvaluatedGenome[T, I, Vector[FV]] =>
      evaluatedPopulation = evaluatedPopulation :+ eg
      //println("Pop size: " + popSize + " EvaluatedPopulation size: " + evaluatedPopulation.size + " evaluatedGenome" + eg)
      if (evaluatedPopulation.size >= popSize) evaluationDone

  }

  def evaluate(g: Genome[T, I]): Unit = {
    val evalator = evalIterator.hasNext match {
      case true => evalIterator.next
      case false =>
        evalIterator = evaluators.iterator
        evalIterator.next
    }
    evalator ! g
  }

  def evaluationDone: Unit = {
    // Combine the archive and new population to calculate SPEA2fitness and update the archive
    val speaPopulation: EvaluatedPopArchive[T, I, SPEA2Fitness[FV]] =
      spea2FitnessEvaluator.evaluatePopArchive(new EvaluatedPopArchive[T, I, Vector[FV]](evaluatedPopulation, Some(currentArchive)))
    popSender ! speaPopulation
  }
}

class SPEA2SpeciesEvaluationActor[T, I <: Iterable[T], F: Ordering](evaluators: List[ActorRef]) extends Actor {
  var evaluatedPopulations: Vector[EvaluatedPopArchive[T, I, F]] = null
  var evalIterator = evaluators.iterator
  var speciesSize = 0
  var speciesSender: ActorRef = null
  def receive = {

      case speciesArchive: SpeciesArchive[T, I, F] =>
        println("Received species for evaluation")
        evaluatedPopulations = Vector[EvaluatedPopArchive[T, I, F]]()
        val popArchives = speciesArchive.popArchives
        speciesSize = popArchives.size
        speciesSender = sender
        popArchives.foreach { popArchive =>
          evaluate(popArchive)
        }
      case epa: EvaluatedPopArchive[T, I, F] =>
        evaluatedPopulations = evaluatedPopulations :+ epa
        //println("Species size: " + speciesSize + " EvaluatedPopulations size: " + evaluatedPopulations.size + " evaluatedPopArchive" + epa)
        if (evaluatedPopulations.size >= speciesSize) evaluationDone


  }

  def evaluate(popArchive: PopArchive[T, I, F]): Unit = {
    val evalator = evalIterator.hasNext match {
      case true => evalIterator.next
      case false =>
        evalIterator = evaluators.iterator
        evalIterator.next
    }
    evalator ! popArchive
  }

  def evaluationDone: Unit = {
    speciesSender ! new EvaluatedSpeciesArchive[T, I, F](evaluatedPopulations)
  }
}
