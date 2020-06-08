sealed trait SinglyLinkedList[+A] {

  def :+[B >: A](el:B): Node[B] = append(el)
  def append[B >: A](el:B): Node[B] = this match {
    case Empty => Node(el, Empty)
    case Node(head, tail) => Node(head, tail.append(el))
  }

  def +:[B >: A](el:B): Node[B] = prepend(el)
  def prepend[B >: A](el:B): Node[B] = this match {
    case Empty => Node(el, Empty)
    case Node(_, _) => Node(el, this)
  }

  def map[B](f: A => B): SinglyLinkedList[B] = this match {
    case Node(head, tail) => Node(f(head), tail.map(f))
    case Empty => Empty
  }

}

object SinglyLinkedList {

  def empty[A]: SinglyLinkedList[A] = Empty

  def apply[A](el: A): SinglyLinkedList[A] = Node(el, Empty)

}

case class Node[A](head: A, tail: SinglyLinkedList[A]) extends SinglyLinkedList[A]

case object Empty extends SinglyLinkedList[Nothing]

object SinglyLinkedListDemo {

  val emptyList = SinglyLinkedList.empty
  println("emptyList: " + emptyList)

  val initList = SinglyLinkedList("hello world")
  println("initList: " + initList)

  val firstAppend = initList :+ "hi there"
  println("firstAppend: " + firstAppend)

  val secondAppend = firstAppend :+ "second append"
  println("secondAppend: " + secondAppend)

  val firstPrepend = "goodbye world" +: secondAppend
  println("firstPrepend: " + firstPrepend)

  val thirdAppend = firstPrepend :+ "last"
  println("thirdAppend: " + thirdAppend)

  val numbers = SinglyLinkedList(1).append(2).append(3)
  println("numbers: " + numbers)
  val doubled = numbers.map(_ * 2)
  println("doubled: " + doubled)

}
