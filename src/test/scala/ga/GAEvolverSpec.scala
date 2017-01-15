package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom
import akka.actor._
import scala.math.Ordering.Double


class GAEvolverSpec extends FlatSpec {
  val random = new SimRandom(0)

  // Evaluate the population so that fitness is the average of the values
  class FirstEvaluator extends SimpleEvaluator[Long, Vector[Long], Double] {
    override def evaluateGenome(g: Genome[Long, Vector[Long]]): EvaluatedGenome[Long, Vector[Long], Double] = {
      new EvaluatedGenome[Long, Vector[Long], Double](g, (g.map(_.asInstanceOf[Long]).sum).toDouble/g.size)
    }
  }
  val ias = LongAlleleSet(random, 1, 10)
  val alleles = (for (i <- 1 to 10) yield ias).toVector

  def initGenome: Genome[Long, Vector[Long]] = alleles.map(_.getRandomValue)
  val pop: Population[Long, Vector[Long]] = (for (i <- 1 to 10) yield initGenome).toVector
  val initialArchive: EvaluatedPopulation[Long, Vector[Long], Double] = Vector()
  val firstGeneration: SpeciesArchive[Long, Vector[Long], Double] = SpeciesArchive(Vector(PopArchive(pop, Some(initialArchive))))

  val evaluator = new FirstEvaluator
  val numCrossoverPoints = 2
  val numChildren = 1
  val mutationRate = 0.1
  val numInTournament = 2
  val replacementRate = 0.5

  val crossover = new MultiPointCrossover[Long](random, numCrossoverPoints, numChildren)
  val mutator = AlleleSetMutator[Long](random, alleles, mutationRate)


  val tournamentSelector = new TournamentSelector[Long, Vector[Long], Double](random, numInTournament)
  val bestSelector = new BestSelector[Long, Vector[Long], Double]
  val archiveUpdater = new BestNArchiveUpdater[Long, Vector[Long], Double](10)
  val evolver = new CrossoverEvolver(mutator, crossover, tournamentSelector, bestSelector, archiveUpdater, replacementRate)

  val randomSelector = new RandomSelector[Long, Vector[Long], Double](random)
  val migrater = new SelectionMigrator[Long, Vector[Long], Double](randomSelector, 0.40)

  class DataActor[T, I <: Iterable[T]] extends Actor {
    var history: Vector[EvaluatedSpeciesArchive[T, I, Double]] = Vector()
    def receive = {
      case esa: EvaluatedSpeciesArchive[T, I, Double] =>
        history = history :+ esa
        printFitnessHistories
        printArchives
        printArchiveFitness
    }

    def printArchiveFitness: Unit = {
      val currentArchives: Option[Vector[EvaluatedPopulation[T, I, Double]]] = history.map(_.archives).last
      currentArchives match {
        case Some(ar) => ar.foreach(ep => println("Archive fitness for generation " + history.size + ": " + ep.map(_.fitness)))
      }

    }

    def printArchives: Unit = {
      val currentArchives: Option[Vector[EvaluatedPopulation[T, I, Double]]] = history.map(_.archives).last
      currentArchives match {
        case Some(ar) =>
          ar.foreach { ep =>
            println("Archive for generation " + history.size + ": ")
            ep.foreach(eg => println(eg.genome.toString()))
          }
      }

    }

    def printFitnessHistories: Unit = {
      val size = history(0).populations.size
      val generation = history.size
      for (i <- 0 to size-1) {
        println("Generation " + generation + " history " + i + "L " + getFitnessHistory(i))
      }
    }

    def getAvgFitness(ep: EvaluatedPopulation[T, I, Double]): Double = (ep.map(_.fitness).sum)/ep.size

    def getPopulationHistory(i: Int): Vector[EvaluatedPopulation[T, I, Double]] = {
      val populations = history.map(_.populations)
      populations.map(_(i))
    }

    def getFitnessHistory(i: Int): Vector[Double] = getPopulationHistory(i).map(getAvgFitness(_))
  }

  val system = ActorSystem("Test")
  val dataActor: ActorRef = system.actorOf(Props(new DataActor))
  val evaluators: List[ActorRef] = (for (i <- 1 to 5) yield {
    system.actorOf(Props(new GenomeEvaluationActor(new FirstEvaluator)), "Evaluator" + i.toString)
  }).toList

  val popEvaluationActor = system.actorOf(Props(new PopulationEvaluationActor[Long, Vector[Long], Double](evaluators)), "PopEvalActor")
  val speciesEvaluationActor = system.actorOf(Props(new SpeciesEvaluationActor(List(popEvaluationActor))), "SpeciesEvalActor")

  "A GA" should "evolve the population" in {
    val ga = system.actorOf(Props(new GA(speciesEvaluationActor, dataActor, migrater, evolver, firstGeneration, 1000)), "ga")
    ga ! Start
    Thread.sleep(10000)
  }

}
