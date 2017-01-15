package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom
import akka.actor._
import sGeneticAlgorithm.ga.pareto._

import scala.math.Ordering.Implicits._


class SPEA2GASpec extends FlatSpec {
  val random = new SimRandom(0)
  // Evaluate the population so that fitness is the average of the values
  implicit val ord: Ordering[SPEA2Fitness[Long]] = new SPEA2FitnessOrdering[Long]()
  val ias = LongAlleleSet(random, 1, 10)
  val alleles = (for (i <- 1 to 2) yield ias).toVector

  def initGenome: Genome[Long, Vector[Long]] = alleles.map(_.getRandomValue)
  val pop: Population[Long, Vector[Long]] = (for (i <- 1 to 10) yield initGenome).toVector
  val initialArchive: EvaluatedPopulation[Long, Vector[Long], SPEA2Fitness[Long]] = Vector()
  val firstGeneration: SpeciesArchive[Long, Vector[Long], SPEA2Fitness[Long]] = SpeciesArchive(Vector(PopArchive(pop, Some(initialArchive))))

  val genomeEvaluator: GenomeEvaluator[Long, Vector[Long], Vector[Long]] = new OnDiagonalFitnessEvaluator
  val numCrossoverPoints = 2
  val numChildren = 1
  val mutationRate = 0.1
  val numInTournament = 2

  val crossover = new MultiPointCrossover[Long](random, numCrossoverPoints, numChildren)
  val mutator = AlleleSetMutator[Long](random, alleles, mutationRate)


  val tournamentSelector = new TournamentSelector[Long, Vector[Long], SPEA2Fitness[Long]](random, numInTournament)

  val bestSelector = new BestSelector[Long, Vector[Long], SPEA2Fitness[Long]]
  val archiveUpdater = new SPEA2ArchiveUpdater[Long, Vector[Long], Long](10)
  val evolver = new SPEA2Evolver[Long, Vector[Long], SPEA2Fitness[Long]](mutator, crossover, tournamentSelector, archiveUpdater, 10)

  val randomSelector = new RandomSelector[Long, Vector[Long], SPEA2Fitness[Long]](random)
  val migrater = new SelectionMigrator[Long, Vector[Long], SPEA2Fitness[Long]](randomSelector, 0.40)

  class DataActor[T, I <: Iterable[T]] extends Actor {
    var history: Vector[EvaluatedSpeciesArchive[T, I, SPEA2Fitness[Long]]] = Vector()
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

  val system = ActorSystem("Test")
  val dataActor: ActorRef = system.actorOf(Props(new DataActor))
  val evaluators: List[ActorRef] = (for (i <- 1 to 5) yield {
    system.actorOf(Props(new GenomeEvaluationActor(new OnDiagonalFitnessEvaluator)), "Evaluator" + i.toString)
  }).toList

  val popEvaluationActor = system.actorOf(Props(new SPEA2PopulationEvaluationActor[Long, Vector[Long], Long](evaluators)), "PopEvalActor")
  val speciesEvaluationActor = system.actorOf(Props(new SPEA2SpeciesEvaluationActor[Long, Vector[Long], SPEA2Fitness[Long]](List(popEvaluationActor))), "SpeciesEvalActor")

  "A GA" should "evolve the population" in {
    val ga = system.actorOf(Props(new GA(speciesEvaluationActor, dataActor, migrater, evolver, firstGeneration, 100)), "ga")
    ga ! Start
    Thread.sleep(20000)
  }

}