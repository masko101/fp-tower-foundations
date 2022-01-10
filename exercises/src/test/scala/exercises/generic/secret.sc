import exercises.generic.GenericFunctionExercises._

secret.swap.map(s=> new String(s.reverse.toArray))

def zipV2[A, B, To](list1: List[A], list2: List[B])(combine: (A, B) => To): List[To] =
  (0 to Math.min(list1.size, list2.size))
    .map(i => combine(list1(i),list2(i))).toList

val list1 = List("1", "2")
val list2 = List(3, 4)
val function: ((String, Int) => Double) => List[Double] =
  zipV2[String, Int, Double](list1, list2)

function((s, i) => s.toDouble * i.doubleValue)

list1.zipAll(list2, 999999, "99999")