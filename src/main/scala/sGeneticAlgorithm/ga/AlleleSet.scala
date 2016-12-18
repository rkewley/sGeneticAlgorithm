package sGeneticAlgorithm.ga

import sGeneticAlgorithm.utils.SimRandom

/**
  * This is an abstract class from which all AlleleSets descend.  An AlleleSet
  * is simply a set of possible values (instances of objects) for a genome to have at a location
  * along its string.
  * @param random Random number generator
  */
abstract class AlleleSet[T](val random: SimRandom) {

  def getRandomValue: T

}


/**
  * This allele set randomly generates integer AlleleValues in the
  * inclusive interval [min, max].
  * @param aRandom Random number generator
  * @param min The minimum value in this allele set
  * @param max The maximum value in this allele set
  */
case class RealAlleleSet(aRandom: SimRandom, val min: Double, val max: Double) extends AlleleSet[Double](aRandom) {
  def getRandomValue = min + (max - min) * random.nextDouble
}

/**
  * This allele set randomly generates integer AlleleValues in the
  * inclusive interval [min, max].
  * @param aRandom Random number generator
  * @param min The minimum integer in this allele set
  * @param max The maximum integer in this allele set
  */
case class LongAlleleSet(aRandom: SimRandom, val min: Long, val max: Long) extends AlleleSet[Long](aRandom) {
  def getRandomValue = min + ((max - min + 1)*random.nextDouble).toLong
}