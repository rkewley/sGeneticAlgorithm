package ga

import org.scalatest._
import sGeneticAlgorithm.ga._
import sGeneticAlgorithm.utils.SimRandom

class AlleleSetSpec extends FlatSpec {
  val random = new SimRandom(0)

  "An integer allele set" should "generate random numbers from min to max" in {
    val ias = LongAlleleSet(random, 0, 2)
    println("The integers from 0 to 2 are: ")
    val ints = for (i <- 1 to 100) yield ias.getRandomValue
    ints.foreach { i =>
      println(i)
      assert(i >= 0 && i <= 2)
    }
    assert(ints.min == 0)
    assert(ints.max == 2)
  }

  "An real allele set" should "generate random numbers from min to max" in {
    val ias = RealAlleleSet(random, 0.0, 2.0)
    println("The integers from 0 to 2 are: ")
    val ints = for (i <- 1 to 100) yield ias.getRandomValue
    ints.foreach { i =>
      println(i)
      assert(i >= 0 && i <= 2)
    }
    assert(ints.min - 0 <= 0.2)
    assert(2 - ints.max <= 0.2)
  }

  "An Initializer" should "initilize a genome with random values" in {
    val ias = LongAlleleSet(random, 0, 2)
    println("The integers from 0 to 2 are: ")
    val alleles = (for (i <- 1 to 10) yield ias).toVector
    val initializer = AllelesInitializer(alleles, 100)
    val pop = initializer.initialize
    val ints = for (g <- pop) yield {
      for (i <- g) yield  i
    }
    val flatInts = ints.flatten
    flatInts.foreach { i =>
      assert(i >= 0 && i <= 2)
    }
    assert(flatInts.size == 1000)
    assert(flatInts.min == 0)
    assert(flatInts.max == 2)
  }
}
