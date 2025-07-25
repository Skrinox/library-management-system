package models
import models.types.*

enum Genre:
  case Fiction, NonFiction, Mystery, Science, SciFi, Biography, History, Fantasy, Romance, 
       Thriller, Horror, Poetry, Children, Classic
  
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

enum Availability:
  case Available, CheckedOut, Reserved
  
  def canBorrow: Boolean = this match
    case Available => true
    case CheckedOut | Reserved => false
  
  def statusMessage: String = this match
    case Available => "Available for borrowing"
    case CheckedOut => "Currently checked out"
    case Reserved => "Reserved for another user"

// Nouvel enum pour les permissions
enum Permission:
  case Read, Write, Delete, Admin
  
  def level: Int = this match
    case Read => 1
    case Write => 2
    case Delete => 3
    case Admin => 4

// Enum pour les types de rapport
enum ReportType:
  case Daily, Weekly, Monthly, Annual
  
  def daysIncluded: Int = this match
    case Daily => 1
    case Weekly => 7
    case Monthly => 30
    case Annual => 365

final case class Book (
  isbn: ISBN,
  title: String,
  authors: List[String],
  publicationYear: Int,
  genre: Genre,
  availability: Availability
                      ):
  def isAvailable: Boolean = availability == Availability.Available
  def display: String = 
    s"Title: $title, Authors: ${authors.mkString(", ")}, Year: $publicationYear, Genre: $genre, ISBN: ${isbn.value}, Availability: $availability"

