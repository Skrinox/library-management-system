package models

import models.types.*

/** Represents literary genres used to classify books. */
enum Genre:
  case Fiction, NonFiction, Mystery, Science, SciFi, Biography, History, Fantasy, Romance, 
       Thriller, Horror, Poetry, Children, Classic

  /** Provides a human-readable description of the genre. */
  def description: String = this match
    case Fiction => "Fiction literature"
    case NonFiction => "Non-fiction works"
    case Mystery => "Mystery and detective stories"
    case Science => "Scientific literature"
    case SciFi => "Science fiction"
    case Biography => "Biographical works"
    case History => "Historical literature"
    case Fantasy => "Fantasy literature"
    case Romance => "Romance novels"
    case Thriller => "Thriller novels"
    case Horror => "Horror stories"
    case Poetry => "Poetry collections"
    case Children => "Children's literature"
    case Classic => "Classic literature"

/** Indicates the availability status of a book. */
enum Availability:
  case Available, CheckedOut, Reserved

  /** Whether the book can be borrowed in this state. */
  def canBorrow: Boolean = this match
    case Available => true
    case CheckedOut | Reserved => false

  /** Provides a status message describing the current state. */
  def statusMessage: String = this match
    case Available => "Available for borrowing"
    case CheckedOut => "Currently checked out"
    case Reserved => "Reserved for another user"

/** Describes permission levels within the system. */
enum Permission:
  case Read, Write, Delete, Admin

  /** Numerical level of authority associated with this permission. */
  def level: Int = this match
    case Read => 1
    case Write => 2
    case Delete => 3
    case Admin => 4

/** Types of time-based reports for library activity. */
enum ReportType:
  case Daily, Weekly, Monthly, Annual

  /** Number of days included in the report range. */
  def daysIncluded: Int = this match
    case Daily => 1
    case Weekly => 7
    case Monthly => 30
    case Annual => 365

/**
 * Represents a book in the library catalog.
 * 
 * @param isbn The unique ISBN identifier (opaque type)
 * @param title The title of the book
 * @param authors A list of authors
 * @param publicationYear Year of publication
 * @param genre Genre of the book
 * @param availability Book status (Available, CheckedOut, Reserved)
 */
final case class Book(
  isbn: ISBN,
  title: String,
  authors: List[String],
  publicationYear: Int,
  genre: Genre,
  availability: Availability
):
  /** Indicates whether the book is currently available for borrowing. */
  def isAvailable: Boolean = availability == Availability.Available

  /** Returns a formatted string with all the book's metadata. */
  def display: String =
    s"Title: $title, Authors: ${authors.mkString(", ")}, Year: $publicationYear, Genre: $genre, ISBN: ${isbn.value}, Availability: $availability"