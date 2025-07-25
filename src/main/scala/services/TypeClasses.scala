package services

import models._
import models.types._

// Type class pour l'affichage formaté
trait Displayable[T]:
  def display(value: T): String

// Given instances pour Displayable
given Displayable[Book] with
  def display(book: Book): String =
    s"${book.title} by ${book.formattedAuthors} (${book.publicationYear})"

given Displayable[User] with
  def display(user: User): String = user match
    case Student(id, name, email, major) => 
      s"Student: $name ($email) - Major: $major"
    case Faculty(id, name, department) => 
      s"Faculty: $name - Department: $department"
    case Librarian(id, name) => 
      s"Librarian: $name"

// Instances spécifiques pour les sous-types de User
given Displayable[Student] with
  def display(student: Student): String =
    s"Student: ${student.name} (${student.email}) - Major: ${student.major}"

given Displayable[Faculty] with
  def display(faculty: Faculty): String =
    s"Faculty: ${faculty.name} - Department: ${faculty.department}"

given Displayable[Librarian] with
  def display(librarian: Librarian): String =
    s"Librarian: ${librarian.name}"

// Type class pour la sérialisation
trait Serializable[T]:
  def serialize(value: T): String

given Serializable[Book] with
  def serialize(book: Book): String =
    s"${book.isbn.value}|${book.title}|${book.authors.mkString(";")}|${book.publicationYear}|${book.genre}|${book.availability}"

given Serializable[User] with
  def serialize(user: User): String = user match
    case Student(id, name, email, major) => 
      s"STUDENT|${id.value}|$name|$email|$major"
    case Faculty(id, name, department) => 
      s"FACULTY|${id.value}|$name|$department"
    case Librarian(id, name) => 
      s"LIBRARIAN|${id.value}|$name"

// Instances spécifiques pour les sous-types de User
given Serializable[Student] with
  def serialize(student: Student): String =
    s"STUDENT|${student.id.value}|${student.name}|${student.email}|${student.major}"

given Serializable[Faculty] with
  def serialize(faculty: Faculty): String =
    s"FACULTY|${faculty.id.value}|${faculty.name}|${faculty.department}"

given Serializable[Librarian] with
  def serialize(librarian: Librarian): String =
    s"LIBRARIAN|${librarian.id.value}|${librarian.name}"

// Type class pour la comparaison
trait Comparable[T]:
  def compare(a: T, b: T): Int

given Comparable[Book] with
  def compare(a: Book, b: Book): Int =
    a.title.compareTo(b.title)

given Comparable[User] with
  def compare(a: User, b: User): Int =
    a.name.compareTo(b.name)

// Fonctions utilitaires utilisant les type classes
object DisplayService:
  def show[T](value: T)(using displayable: Displayable[T]): String =
    displayable.display(value)
  
  def showList[T](values: List[T])(using displayable: Displayable[T]): List[String] =
    values.map(displayable.display)

object SerializationService:
  def save[T](value: T)(using serializable: Serializable[T]): String =
    serializable.serialize(value)
  
  def saveAll[T](values: List[T])(using serializable: Serializable[T]): List[String] =
    values.map(serializable.serialize)

object SortingService:
  def sort[T](values: List[T])(using comparable: Comparable[T]): List[T] =
    values.sortWith((a, b) => comparable.compare(a, b) < 0)

// Type class pour la validation
trait Validator[T]:
  def validate(value: T): Either[String, T]

given Validator[Book] with
  def validate(book: Book): Either[String, Book] =
    if book.title.trim.isEmpty then Left("Title cannot be empty")
    else if book.authors.isEmpty then Left("Book must have at least one author")
    else if book.publicationYear <= 0 then Left("Invalid publication year")
    else Right(book)

given Validator[User] with
  def validate(user: User): Either[String, User] = user match
    case Student(_, name, email, major) =>
      if name.trim.isEmpty then Left("Student name cannot be empty")
      else if !email.contains("@") then Left("Invalid email")
      else if major.trim.isEmpty then Left("Major cannot be empty")
      else Right(user)
    case Faculty(_, name, department) =>
      if name.trim.isEmpty then Left("Faculty name cannot be empty")
      else if department.trim.isEmpty then Left("Department cannot be empty")
      else Right(user)
    case Librarian(_, name) =>
      if name.trim.isEmpty then Left("Librarian name cannot be empty")
      else Right(user)