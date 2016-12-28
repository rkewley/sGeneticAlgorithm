package sGeneticAlgorithm.ga

import akka.actor._
import GA._

class GenomeEvaluationActor[T, I <: Iterable[T], F: Ordering](genomeEvaluator: GenomeEvaluator[T, I, F]) extends Actor {
    def receive = {
        case g: Genome[T, I] => sender ! genomeEvaluator.evaluateGenome(g)
    }
}

class PopulationEvaluationActor[T, I <: Iterable[T], F: Ordering](evaluators: List[ActorRef], archiveUpdaterOption: Option[ArchiveUpdater[T, I, F]] = None) extends Actor {
    var evaluatedPopulation: EvaluatedPopulation[T, I, F] = null
    var evalIterator = evaluators.iterator
    var archiveOption: Option[EvaluatedPopulation[T, I, F]] = None
    var popSize = 0
    var popSender: ActorRef = null
    def receive = {
        case popArchive: PopArchive[T, I, F] =>
            println("Received population for evaluation")
            val pop = popArchive.pop
            val arOption = popArchive.archive
            evaluatedPopulation = Vector[EvaluatedGenome[T, I, F]]()
            popSize = pop.size
            popSender = sender
            pop.foreach { genome =>
                evaluate(genome)
            }
            archiveOption = arOption
        case eg: EvaluatedGenome[T, I, F] =>
            evaluatedPopulation = evaluatedPopulation :+ eg
            println("Pop size: " + popSize + " EvaluatedPopulation size: " + evaluatedPopulation.size + " evaluatedGenome" + eg)
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
        
        val newArchiveOption: Option[EvaluatedPopulation[T, I, F]] = archiveOption match {
            case None => None
            case Some(archive) => Some(archiveUpdaterOption.get.updateArchive(archive, evaluatedPopulation))
        }
        popSender ! EvaluatedPopArchive[T, I, F](evaluatedPopulation, newArchiveOption)
    }
}


