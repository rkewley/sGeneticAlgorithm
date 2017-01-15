package sGeneticAlgorithm.ga

import akka.actor._
import GA._

class GenomeEvaluationActor[T, I <: Iterable[T], F: Ordering](genomeEvaluator: GenomeEvaluator[T, I, F]) extends Actor {
    def receive = {
        case g: Genome[T, I] => sender ! genomeEvaluator.evaluateGenome(g)
    }
}

class PopulationEvaluationActor[T, I <: Iterable[T], F: Ordering](val evaluators: List[ActorRef]) extends Actor {
    var evaluatedPopulation: EvaluatedPopulation[T, I, F] = null
    var evalIterator = evaluators.iterator
    var popSize = 0
    var popSender: ActorRef = null
    def receive = {
        case pop: Population[T, I] =>
            //println("Received population for evaluation")
            evaluatedPopulation = Vector[EvaluatedGenome[T, I, F]]()
            popSize = pop.size
            popSender = sender
            pop.foreach { genome =>
                evaluate(genome)
            }
        case eg: EvaluatedGenome[T, I, F] =>
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
        popSender ! evaluatedPopulation
    }
}

class SpeciesEvaluationActor[T, I <: Iterable[T], F: Ordering](evaluators: List[ActorRef]) extends Actor {
    var evaluatedPopulations: Vector[EvaluatedPopulation[T, I, F]] = null
    var evalIterator = evaluators.iterator
    var speciesSize = 0
    var speciesSender: ActorRef = null
    def receive = {
        case v: Vector[_] => v.head match {

            case p: Population[T, I] =>
                val species: Species[T, I] = v.asInstanceOf[Species[T, I]]
                println("Received species for evaluation")
                evaluatedPopulations = Vector[EvaluatedPopulation[T, I, F]]()
                speciesSize = species.size
                speciesSender = sender
                species.foreach { population =>
                    evaluate(population)
                }
            case eg: EvaluatedGenome[T, I, F] =>
                val ep = v.asInstanceOf[EvaluatedPopulation[T, I, F]]
                evaluatedPopulations = evaluatedPopulations :+ ep
                //println("Species size: " + speciesSize + " EvaluatedPopulations size: " + evaluatedPopulations.size + " evaluatedPopArchive" + epa)
                if (evaluatedPopulations.size >= speciesSize) evaluationDone
        }

    }

    def evaluate(pop: Population[T, I]): Unit = {
        val evalator = evalIterator.hasNext match {
            case true => evalIterator.next
            case false =>
                evalIterator = evaluators.iterator
                evalIterator.next
        }
        evalator ! pop
    }

    def evaluationDone: Unit = {
        speciesSender ! evaluatedPopulations
    }
}

/*  Fix this to evaluate but not update archive
class EcosystemEvaluationActor(evaluators: Vector[ActorRef]) extends Actor {
    var evaluatedSpeciesArchives: Vector[EvaluatedSpeciesArchive[_, _ <: Iterable[_], _]] = null
    var ecosystemSize = 0
    var ecosystemSender: ActorRef = null
    def receive = {
        case ecosystemArchive: EcosystemArchive =>
            println("Received ecosystem for evaluation")
            evaluatedSpeciesArchives = Vector[EvaluatedSpeciesArchive[_, _ <: Iterable[_], _]]()
            ecosystemSize = ecosystemArchive.speciesArchives.size
            ecosystemSender = sender
            for (i <- 0 to ecosystemArchive.speciesArchives.size - 1) {
                evaluators(i) ! ecosystemArchive.speciesArchives(i)
            }
        case esa: EvaluatedSpeciesArchive[_, _, _] =>
            evaluatedSpeciesArchives = evaluatedSpeciesArchives :+ esa
            println("Ecosystem size: " + ecosystemSize + " EvaluatedSpeciesArchives size: " + evaluatedSpeciesArchives.size + " evaluatedSpeciesArchive: + esa")
            if (evaluatedSpeciesArchives.size >= ecosystemSize) evaluationDone

    }

    def evaluationDone: Unit = {
        val evaluatedEcosystemArchive = EvaluatedEcosysteemArchive(evaluatedSpeciesArchives)
        ecosystemSender ! evaluatedEcosystemArchive
    }
}

*/