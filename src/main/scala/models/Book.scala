package models
import models.types.*

enum Genre:
  case Fiction, NonFiction, Mystery, Science, SciFi, Biography, History, Fantasy, Romance, 
       Thriller, Horror, Poetry, Children, Classic
  
enum Availability:
  case Available, CheckedOut, Reserved

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
    
