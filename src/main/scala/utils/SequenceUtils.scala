package utils

object SequenceUtils {
  def permutations[A](first: Seq[A], second: Seq[A]): Set[Seq[A]] = (first.toList, second.toList) match
    case (Seq(), second) => Set(second)
    case (first, Seq()) => Set(first)
    case (f::fs, s::ss) => permutations(fs, s +: ss).map(f +: _) ++ permutations(f +: fs, ss).map(s +: _)
  
  def flip[A, B](pairs: Seq[(A, B)]): Seq[(B, A)] =
    pairs.map((a, b) => (b, a))
}
