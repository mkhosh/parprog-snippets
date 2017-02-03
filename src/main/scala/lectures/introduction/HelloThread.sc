class HelloThread(m: String) extends Thread {
  override def run(): Unit = {
    println("Hello! " + m)
    println("World! " + m)
  }
}

val t = new HelloThread("t")
val s = new HelloThread("s")

t.start()
s.start()
s.join()
t.join()

object test {
  private val x = new AnyRef {}
  private var uidCount = 0L

  def getUniqueId: Long = x.synchronized {
    uidCount += 1
    uidCount
  }

  def startThread() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId
        println(uids)
      }
    }
    t.start()
    t
  }
}

val t1 = test.startThread()
val t2 = test.startThread()
t2.join()
t1.join()

