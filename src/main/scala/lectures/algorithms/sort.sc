import scala.util.Sorting

class A {
  private def cat(s1: String, s2: String) = s1 + " " + s2
}

val a = new A
val hi = "Hello"
val all = "World"
hi.getClass
val method = a.getClass.getDeclaredMethod("cat", hi.getClass, all.getClass)
method.setAccessible(true)
method.invoke(a, hi.asInstanceOf[AnyRef], all)



