object OptionTest extends App {
  val myOption1 = Some(1)
  val myOption2: Option[Int] = None

  println(myOption1.map(_ + 1))
  println(myOption2.map(_ + 1))
  println(myOption1.getOrElse(2))
  println(myOption2.getOrElse(2))
  println(myOption1.flatMap(x => Some(x+1)))
  println(myOption2.flatMap(x => Some(x+1)))
  println(myOption1.orElse(Some(2)))
  println(myOption2.orElse(Some(2)))
  println(myOption1.filter(_ == 2))
  println(myOption2.filter(_ == 2))

}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(x => f(x)).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]