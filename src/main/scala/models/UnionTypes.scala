package models

import models.types._
import services.LibraryError

object SearchQuery:
  type SearchCriteria = String | Genre | Availability | UserID
  type SearchResult = Book | User | String
  
  def searchByString(query: String, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.title.toLowerCase.contains(query.toLowerCase))
  
  def searchByGenre(genre: Genre, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.genre == genre)
  
  def searchByAvailability(availability: Availability, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.availability == availability)
  
  def searchByUserId(userId: UserID, catalog: LibraryCatalog): List[User] =
    catalog.users.filter(_.id.value == userId.value)
  
  def search(criteria: SearchCriteria, catalog: LibraryCatalog): List[SearchResult] =
    if criteria.isInstanceOf[String] then
      searchByString(criteria.asInstanceOf[String], catalog)
    else if criteria.isInstanceOf[Genre] then
      searchByGenre(criteria.asInstanceOf[Genre], catalog)
    else if criteria.isInstanceOf[Availability] then
      searchByAvailability(criteria.asInstanceOf[Availability], catalog)
    else
      searchByUserId(criteria.asInstanceOf[UserID], catalog)

type LibraryValidationError = String | IllegalArgumentException | NumberFormatException

object ValidationResult:
  type Result[T] = T | LibraryValidationError
  
  def success[T](value: T): Result[T] = value
  def error(message: String): Result[Nothing] = message