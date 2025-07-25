package utils

trait Show[A]:
  def show(a: A): String

object Show:
  given Show[Int] with
    def show(a: Int): String = s"Int($a)"

  given Show[models.Book] with
    def show(b: models.Book): String = 
      s"${b.title} by ${b.authors.mkString(", ")} [${b.isbn.value}]"

  def printShow[A](a: A)(using sh: Show[A]): Unit =
    println(sh.show(a))