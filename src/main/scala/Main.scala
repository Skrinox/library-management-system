package utils

import models._
import models.types._
import models.Genre._
import models.Availability._
import models.TransactionType
import services._
import java.time.LocalDateTime
import scala.io.StdIn.readLine

object Main:

  def main(args: Array[String]): Unit =
    var catalog = LibraryCatalog(Nil, Nil, Nil)
    var running = true

    while running do
      println(
        """
          |====== Library System ======
          |1. List all books
          |2. Add a book
          |3. Remove a book
          |4. Register a user
          |5. Borrow a book
          |6. Return a book
          |7. Show transactions
          |8. Quit
          |===========================
          |""".stripMargin)

      readLine("Choose an option: ") match
        case "1" =>
          if catalog.books.isEmpty then println("No books in the catalog.")
          else catalog.books.foreach(b => println(b.display))

        case "2" =>
          println("Enter ISBN (format XXX-X-XXXXXX-XXX-X):")
          val isbn = readLine()
          println("Enter title:")
          val title = readLine()
          println("Enter authors (comma separated):")
          val authors = readLine().split(",").map(_.trim).toList
          println("Enter publication year:")
          val year = readLine().toIntOption.getOrElse(0)
          println("Choose genre (e.g. Fiction, SciFi, Classic):")
          val genreInput = readLine()
          val genre = Genre.values.find(_.toString.equalsIgnoreCase(genreInput)).getOrElse(Fiction)

          val book = Book(ISBN(isbn), title, authors, year, genre, Available)
          Validation.validateBook(book) match
            case Right(validBook) =>
              catalog = catalog.addBook(validBook)
              println("Book added.")
            case Left(error) =>
              println(s"Invalid book: $error")

        case "3" =>
          println("Enter ISBN to remove:")
          val isbnStr = readLine()
          isbnStr.toISBN match
            case Some(isbn) =>
              catalog = catalog.removeBook(isbn)
              println("Book removed (if it existed).")
            case None =>
              println("Invalid ISBN format.")

        case "4" =>
          println("Choose user type: 1. Student, 2. Faculty, 3. Librarian")
          readLine() match
            case "1" =>
              val id = readLine("ID: ").toUserID
              val name = readLine("Name: ")
              val email = readLine("Email: ")
              val major = readLine("Major: ")
              val student = Student(id, name, email, major)
              Validation.validateUser(student) match
                case Right(u) => catalog = catalog.addUser(u); println("Student registered.")
                case Left(e) => println(s"Invalid: $e")

            case "2" =>
              val id = readLine("ID: ").toUserID
              val name = readLine("Name: ")
              val dept = readLine("Department: ")
              val faculty = Faculty(id, name, dept)
              Validation.validateUser(faculty) match
                case Right(u) => catalog = catalog.addUser(u); println("Faculty registered.")
                case Left(e) => println(s"Invalid: $e")

            case "3" =>
              val id = readLine("ID: ").toUserID
              val name = readLine("Name: ")
              val librarian = Librarian(id, name)
              Validation.validateUser(librarian) match
                case Right(u) => catalog = catalog.addUser(u); println("Librarian registered.")
                case Left(e) => println(s"Invalid: $e")

            case _ => println("Invalid user type.")

        case "5" =>
          println("Enter user ID:")
          val userId = readLine().toUserID
          println("Enter book ISBN:")
          val isbnStr = readLine()
          isbnStr.toISBN match
            case Some(isbn) =>
              ErrorHandling.validateTransaction(catalog, isbn, userId) match
                case Right((book, user)) if user.canBorrow =>
                  val updatedBook = book.checkout
                  catalog = catalog.removeBook(book.isbn).addBook(updatedBook)
                  val tx = Transaction(updatedBook, user, LocalDateTime.now(), TransactionType.Borrow)
                  catalog = catalog.addTransaction(tx)
                  println("Book borrowed successfully.")
                case Left(err) =>
                  println(s"Cannot borrow: ${err.message}")
                case _ =>
                  println("User not allowed to borrow.")
            case None => println("Invalid ISBN.")

        case "6" =>
          println("Enter book ISBN to return:")
          val isbnStr = readLine()
          isbnStr.toISBN match
            case Some(isbn) =>
              catalog.findBookByISBN(isbn) match
                case Some(book) =>
                  val returned = book.makeAvailable
                  catalog = catalog.removeBook(book.isbn).addBook(returned)
                  println("Book marked as returned.")
                case None => println("Book not found.")
            case None => println("Invalid ISBN.")

        case "7" =>
          if catalog.transactions.isEmpty then println("No transactions.")
          else catalog.transactions.foreach(tx =>
            println(s"${tx.transactionType} - ${tx.book.title} by ${tx.user.name} at ${tx.timestamp}")
          )

        case "8" =>
          println("Bye :3!")
          running = false

        case _ =>
          println("Invalid option.")
