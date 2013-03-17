package utils
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class KDTreeTest extends Specification {
  "Finds closest point from a random collection" in {
    val rng: Random = new Random(42);
    val allPoints: IndexedSeq[Foo] = (for (i <- 1 to 10000) yield new Foo(randomVector(rng)));
    val tree: KDTree[Foo] = KDTree.create(allPoints);

    val outcomes: Seq[Boolean] = for (
      i <- 1 to 100;
      point = randomVector(rng);
      closestPointFromTree: Foo = KDTree.closestPoint(tree, point);
      closestPointByBruteForce: Foo = allPoints.minBy(f => distance(point, f))
    ) yield closestPointFromTree == closestPointByBruteForce;

    outcomes.forall(x => x);
  }

  private def distance(p: KDTree.Vector, f: Foo): Double = {
    val dx = p(0) - f.vector(0);
    val dy = p(1) - f.vector(1);
    val dz = p(2) - f.vector(2);
    dx * dx + dy * dy + dz * dz;
  }

  private def randomVector(rng: Random): KDTree.Vector =
    (for (i <- 1 to 3) yield rng.nextDouble());

  private class Foo(val vector: KDTree.Vector) extends KDTree.HasVector {
    override def toString = vector.toString;
  }
}