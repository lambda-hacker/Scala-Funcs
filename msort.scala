
object msort extends App {
  def merge(a:List[Int], b:List[Int]):List[Int] = {
    (a, b) match {
    case (l1, Nil) => l1
    case (Nil, l2) => l2
    
    case (h1::t1, h2::t2) => if (h1 <= h2) h1::merge(t1, b) else h2::merge(a, t2)
    }
  } 

  def msort(xs:List[Int]):List[Int] = {
    xs match {
      case Nil => Nil
      case (h::t) => if (t == Nil) xs else {
	val m = xs.length / 2
	val (l, r) = xs splitAt(m)
	merge(msort(l), msort(r))
      }
    }
  }

  merge(List(7, 2), List(1, 9, 3, 5))

}
