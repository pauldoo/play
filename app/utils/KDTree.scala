package utils
import org.apache.commons.lang3.Validate
import scala.collection.mutable.PriorityQueue

object KDTree {
  type Vector = IndexedSeq[Double];

  trait HasVector {
    val vector: Vector;
  }

  def create[A <: HasVector](values: Seq[A]): KDTree[A] = {

    if (values.isEmpty) {
      null;
    } else {

      def map2[A, B, C](f: (A, B) => C)(v1: Seq[A], v2: Seq[B]): IndexedSeq[C] =
        (for ((x, y) <- (v1 zip v2)) yield f(x, y)).toIndexedSeq;

      val vecs: Seq[KDTree.Vector] = values.map { _.vector };

      val min: (Double, Double) => Double = scala.math.min;
      val max: (Double, Double) => Double = scala.math.max;
      val sub: (Double, Double) => Double = _ - _;

      val mins: KDTree.Vector = vecs.reduce(map2(min));
      val maxs: KDTree.Vector = vecs.reduce(map2(max));

      val sizes: KDTree.Vector = map2(sub)(maxs, mins);
      val maxSize = sizes.max;
      val maxDimension = sizes.indexWhere(_ == maxSize);

      val sortedValues: IndexedSeq[A] = values.sortBy { _.vector(maxDimension) }.toIndexedSeq;
      val pivotIndex = sortedValues.length / 2;

      new KDTree[A]( //
        mins, //
        maxs, //
        sortedValues(pivotIndex), //
        maxDimension, //
        create(sortedValues.take(pivotIndex)), //
        create(sortedValues.takeRight(sortedValues.length - pivotIndex - 1)));
    }
  }

  def closestPoint[A <: HasVector](tree: KDTree[A], point: Vector): A = {

    def distanceFn(t: KDTree[A]) = distanceToBounds(point, t);

    class TreeWithMinDistance(val tree: KDTree[A]) {
      val minDistance = distanceFn(tree);
    }
    val queue: PriorityQueue[TreeWithMinDistance] =
      new PriorityQueue[TreeWithMinDistance]()(Ordering.by(p => -p.minDistance));

    def considerEnqueue(t: KDTree[A]) = {
      if (t != null) {
        queue.enqueue(new TreeWithMinDistance(t));
      }
    }
    considerEnqueue(tree);

    def go(bestResult: Tuple2[A, Double]): A = {
      //print(".");

      if (!queue.isEmpty) {
        val currentItem: TreeWithMinDistance = queue.dequeue();

        if (bestResult == null || currentItem.minDistance < bestResult._2) {
          val distanceToPivot = distanceToPoint(point, currentItem.tree.pivot.vector);

          considerEnqueue(currentItem.tree.left);
          considerEnqueue(currentItem.tree.right);

          if (bestResult == null || distanceToPivot < bestResult._2) {
            return go((currentItem.tree.pivot, distanceToPivot));
          } else {
            return go(bestResult);
          }
        } else {
          return bestResult._1;
        }
      } else {
        return bestResult._1;
      }
    };

    val result = go(null);
    //println();
    result;
  }

  private def distanceToPoint(t: Vector, u: Vector): Double = {
    val tI = t.iterator;
    val uI = u.iterator;
    var tot = 0.0;
    while (tI.hasNext) {
      tot += { val d = tI.next() - uI.next(); d * d };
    }
    math.sqrt(tot);
  }

  private def distanceToBounds[A <: HasVector](p: Vector, tree: KDTree[A]): Double = {
    val pI = p.iterator;
    val minI = tree.mins.iterator;
    val maxI = tree.maxs.iterator;
    var tot = 0.0;
    while (pI.hasNext) {
      tot += {
        val t = pI.next();
        val k = math.max(minI.next(), math.min(t, maxI.next()));
        val d = t - k;
        d * d;
      }
    }
    math.sqrt(tot);
  }

  /*
  private def closestPointFromBounds[A <: HasVector](p: Vector, tree: KDTree[A]): Vector = {
    for (i <- 0 until p.size)
      yield math.max(tree.mins(i), math.min(p(i), tree.maxs(i)));
  }
  */
  //(tree.mins, p, tree.maxs).zipped map { (a, b, c) => (math.max(a, math.min(b, c))) }
}

class KDTree[A <: KDTree.HasVector] private ( //
  val mins: KDTree.Vector, //
  val maxs: KDTree.Vector, //
  val pivot: A, //
  val axis: Int, //
  val left: KDTree[A], //
  val right: KDTree[A]) {
}

