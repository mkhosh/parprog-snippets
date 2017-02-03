class Account(val id: Int, private var amount: Int = 0) {

  override def toString: String = s"Amount: $amount"

  private def blockAndtransfer(target: Account, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }

  def transferTo(target: Account, n: Int): Unit = {
    if (id > target.id) this.blockAndtransfer(target, n)
    else target.blockAndtransfer(this, -n)
  }
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transferTo(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(1,500000)
val a2 = new Account(2,700000)

val t = startThread(a1, a2, 150000)
val s = startThread(a2, a1, 150000)
t.join()
t.join()
a1
a2