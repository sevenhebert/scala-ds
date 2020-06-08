// First in first out
case class Queue[A](deq: List[A], enq: List[A]) {

  def enqueue(el: A): Queue[A] = {
    val Queue(deq, enq) = this
    Queue(deq, el :: enq)
  }

  // when dequeue is empty, reverse enqueue (FIFO) and set as dequeue, set enqueue to Nil
  def maybeDequeue: Option[(A, Queue[A])] = this match {
    case Queue(Nil, Nil) => None
    case Queue(dh :: dt, enq) => Some(dh, Queue(dt, enq))
    case Queue(Nil, enq) =>
      val dh :: dt = enq.reverse
      Some(dh, Queue(dt, Nil))
  }

}

object Queue {

  def empty[A]: Queue[A] = Queue(Nil, Nil)

  def apply[A](el: A): Queue[A] = Queue(List(el), Nil)

}

object QueueDemo {

  val emptyQueue = Queue.empty
  println("emptyQueue: " + emptyQueue)

  val firstEnqueue = Queue(1)
  println("firstEnqueue: " + firstEnqueue)

  val secondEnqueue = firstEnqueue.enqueue(2)
  println("secondEnqueue: " + secondEnqueue)

  val thirdEnqueue = secondEnqueue.enqueue(3)
  println("thirdEnqueue: " + thirdEnqueue)

  val firstDequeue = thirdEnqueue.maybeDequeue
  println("firstDequeue: " + firstDequeue)

  val secondDequeue = firstDequeue.get._2.maybeDequeue
  println("secondDequeue: " + secondDequeue)

  val forthEnqueue = secondDequeue.get._2.enqueue(4).enqueue(5).enqueue(6)
  println("forthEnqueue: " + forthEnqueue)

  val thirdDequeue = forthEnqueue.maybeDequeue
  println("thirdDequeue: " + thirdDequeue)

  val forthDequeue = thirdDequeue.get._2.maybeDequeue
  println("forthDequeue: " + forthDequeue)

}
