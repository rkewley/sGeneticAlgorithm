package ga

import org.scalatest.FlatSpec
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom
import akka.actor._
import scala.math.Ordering._


class EvaluationActorSpec extends FlatSpec {
  var evaluated = false
  // Create a population with 3 genomes of 5 identical menmbers each
  val population: Population[Long, Vector[Long]] = (for (i <- 0 to 2) yield {
    (for (j <- 1 to 5) yield i.toLong).toVector
  }).toVector

  // Evaluate the population so that fitness is the average of the values
  class FirstEvaluator extends SimpleEvaluator[Long, Vector[Long], Double] {
    override def evaluateGenome(g: Genome[Long, Vector[Long]]): EvaluatedGenome[Long, Vector[Long], Double] = {
      new EvaluatedGenome[Long, Vector[Long], Double](g, g.sum/g.size)
    }
  }
  
  val system = ActorSystem("Test")

  val evaluators: List[ActorRef] = (for (i <- 1 to 5) yield {
      system.actorOf(Props(new GenomeEvaluationActor(new FirstEvaluator)))
  }).toList
  
  val popEvaluationActor = system.actorOf(Props(new PopulationEvaluationActor[Long, Vector[Long], Double](evaluators)))
  
  var evaluatedPopulation: EvaluatedPopulation[Long, Vector[Long], Double] = null
  
  class TestEvalActor(popArchive: PopArchive[Long, Vector[Long], Double]) extends Actor {
      popEvaluationActor ! popArchive
      def receive = {
          case e: EvaluatedPopArchive[Long, Vector[Long], Double] =>
          //println(e.evaluatedPop)
          println("Checking evaluated population")
          for (i <- 0 to e.evaluatedPop.size - 1) {
              val g = e.evaluatedPop(i)
              println("Fitness is " + g.fitness + " for " + g.genome)
          }
          assert(e.evaluatedPop(0).fitness == e.evaluatedPop(0).genome(0))
          assert(e.evaluatedPop(1).fitness == e.evaluatedPop(1).genome(0))
          assert(e.evaluatedPop(2).fitness == e.evaluatedPop(2).genome(0))
          evaluated = true
      }
  }
  "An Evaluation Actor" should "evaluate all member of the population in parallel" in {
    system.actorOf(Props(new TestEvalActor(PopArchive(population))))
    Thread.sleep(5000)
    assert(evaluated == true)
  }
}
