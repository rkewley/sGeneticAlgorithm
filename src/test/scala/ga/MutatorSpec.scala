package ga

import org.scalatest._
import sGeneticAlgorithm.ga.GA._
import sGeneticAlgorithm.ga.{AlleleSetMutator, LongAlleleSet}
import sGeneticAlgorithm.utils.SimRandom

class MutatorSpec extends FlatSpec {
  val random = new SimRandom(0)

  "A Mutator" should "change some genomem values" in {
    val g: Genome[Any, Vector[Any]] = (for (i <- 1 to 100) yield 0.toLong).toVector
    val ias = LongAlleleSet(random, 0, 2)
    val alleles = (for (i <- 1 to 100) yield ias).toVector
    val mutator = AlleleSetMutator[Any](random, alleles, 0.1)
    val mutant = mutator.mutate(g)
    println("Mutated vector should not be all zeros\n" + mutant)

    assert(mutant.asInstanceOf[Vector[Long]].contains(1.toLong))
    assert(mutant.asInstanceOf[Vector[Long]].contains(2.toLong))
  }

}
