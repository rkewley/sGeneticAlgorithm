type Genome[+T, I <: Iterable[T]] = I

class FitnessData[T, I <: Iterable[T], F <: Ordered[F]](val genome: Genome[T, I], val fitness: F) extends Ordered[FitnessData[T, I, F]] {

  override def compare(that: FitnessData[T, I, F]): Int = {
    fitness.compare(that.fitness)
  }
}

val g: Genome[java.io.Serializable, Vector[java.io.Serializable]] = Vector("aa", "bb", "cc")

//new FitnessData[java.io.Serializable, Vector[java.io.Serializable], Int](g, 2)

1.0.compare(5.0)
Vector(1.0, 2.0, 3.0).max




