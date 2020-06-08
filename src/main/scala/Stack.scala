// Last in first out
case class Stack[A](ls: List[A]) {

  def enqueue(el: A): Stack[A] = Stack(el :: this.ls)

  def maybeDequeue: Option[(A, Stack[A])] = this match {
    case Stack(Nil) => None
    case Stack(h :: t) => Some(h, Stack(t))
  }

  def maybePeek: Option[A] = this match {
    case Stack(Nil) => None
    case Stack(h :: _) => Some(h)
  }

}

object Stack {

def empty[A]: Stack[A] = Stack(Nil)

def apply[A](el: A): Stack[A] = Stack.empty.enqueue(el)

}

object StackDemo {

  val emptyStack = Stack.empty

  val firstEnqueue = Stack("a")

  val fourthEnqueue = firstEnqueue.enqueue("b").enqueue("c").enqueue("d")
  println("fourthEnqueue: " + fourthEnqueue)

  val firstDequeue = fourthEnqueue.maybeDequeue
  println("firstDequeue: " + firstDequeue)

  println("peekAfterFirstDequeue: " + firstDequeue.get._2.maybePeek)

  val secondDequeue = firstDequeue.get._2.maybeDequeue
  println("secondDequeue: " + secondDequeue)

  val fifthEnqueue = secondDequeue.get._2.enqueue("e")
  println("fifthEnqueue: " + fifthEnqueue)

  val thirdDequeue = fifthEnqueue.maybeDequeue
  println("thirdDequeue: " + thirdDequeue)

}