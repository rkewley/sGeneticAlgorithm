package sGeneticAlgorithm.ga

/**
  * This class is a subper class for a class that has fitness data from an evaluation of a Genome
  */

import  GA._

class AlleleValueFitness[T, F](genome: Genome[T, Vector[T]], fitness: F) extends EvaluatedGenome[T, Vector[T], F](genome, fitness)

