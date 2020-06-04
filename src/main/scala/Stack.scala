object Queue {

  def apply[T](items: T*): Queue[T] = items match {
    case Nil => Empty
    case _   => Node(items.head, apply(items.tail: _*))
  }

}

sealed trait Queue[+T] {

  def enqueue[B >: T <: AnyVal](data: B): Queue[B] = this match {
    case Node(head, tail) => Node(head, tail.enqueue(data))
    case Empty            => Node(data, Empty)
  }

  def dequeue[B >: T <: AnyVal]: Queue[B] = this match {
    case Node(_, tail) => tail
    case Empty         => Empty
  }

  def maybeNext[B >: T]: Option[B] = this match {
    case Node(head, _) => Option(head)
    case Empty         => None
  }

  def map[B](f: T => B): Queue[B] = this match {
    case Node(head, tail) => Node(f(head), tail.map(f))
    case Empty            => Empty
  }

}

case class Node[+T](head: T, tail: Queue[T]) extends Queue[T]

case object Empty extends Queue[Nothing]


object Test {

  val x = Queue(1, 2, 3)
  val y = x.enqueue(4).enqueue(5).enqueue(6)
  val z = y.dequeue.dequeue.dequeue

}
