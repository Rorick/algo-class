import scala.collection.immutable

val s = "1 0 1 1"

val v = Integer parseInt (s replace (" ", ""), 2)

Integer.toString(v, 2)

def oneBitDiffs(v: Integer) = {
  for (i <- 0 to 3) yield v ^ 1 << i
}

oneBitDiffs(v) filter (_ > v) map (Integer.toString(_, 2))

val function: (immutable.IndexedSeq[Int]) => immutable.IndexedSeq[Int] = Function chain Seq.fill(2)(oneBitDiffs)

(Function chain Seq.fill(2)(oneBitDiffs)) (List(v))