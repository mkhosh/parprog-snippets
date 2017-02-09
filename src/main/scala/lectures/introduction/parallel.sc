import scala.annotation.tailrec

def power(x: Int, p: Double) =
  math.exp(p * math.log(math.abs(x))).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  @tailrec
  def helper(i: Int, agg: Int): Int =
    if (i >= t) agg
    else helper(i + 1, agg + power(a(i), p))

  helper(s, 0)
}

sumSegment(Array(1,1,1,1),.5,2,4)