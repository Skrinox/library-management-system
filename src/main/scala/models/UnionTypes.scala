package models

import models.types._
import services.LibraryError

/**
 * Contains utilities for querying the library catalog based on flexible criteria.
 */
object SearchQuery:

  /**
   * Represents the allowed input types for searching the catalog.
   * Can be a String (e.g., title), a Genre, an Availability status, or a UserID.
   */
  type SearchCriteria = String | Genre | Availability | UserID

  /**
   * Represents the possible types returned by a search.
   * Can be a Book, a User, or a plain string (e.g., message or ID).
   */
  type SearchResult = Book | User | String

  /**
   * Searches for books by matching their title against a given string (case-insensitive).
   *
   * @param query The string to search for in titles
   * @param catalog The library catalog
   * @return A list of matching books
   */
  def searchByString(query: String, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.title.toLowerCase.contains(query.toLowerCase))

  /**
   * Searches for books by genre.
   *
   * @param genre The genre to filter by
   * @param catalog The library catalog
   * @return A list of books in the given genre
   */
  def searchByGenre(genre: Genre, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.genre == genre)

  /**
   * Searches for books by their availability status.
   *
   * @param availability The desired availability (Available, CheckedOut, Reserved)
   * @param catalog The library catalog
   * @return A list of books matching the given availability
   */
  def searchByAvailability(availability: Availability, catalog: LibraryCatalog): List[Book] =
    catalog.books.filter(_.availability == availability)

  /**
   * Searches for users by their user ID.
   *
   * @param userId The user ID to look for
   * @param catalog The library catalog
   * @return A list containing the matching user, if any
   */
  def searchByUserId(userId: UserID, catalog: LibraryCatalog): List[User] =
    catalog.users.filter(_.id.value == userId.value)

  /**
   * Generic search function that dispatches to the appropriate search method
   * based on the type of the criteria provided.
   *
   * @param criteria The search criteria (title string, genre, availability, or user ID)
   * @param catalog The library catalog
   * @return A list of results matching the criteria
   */
  def search(criteria: SearchCriteria, catalog: LibraryCatalog): List[SearchResult] =
    if criteria.isInstanceOf[String] then
      searchByString(criteria.asInstanceOf[String], catalog)
    else if criteria.isInstanceOf[Genre] then
      searchByGenre(criteria.asInstanceOf[Genre], catalog)
    else if criteria.isInstanceOf[Availability] then
      searchByAvailability(criteria.asInstanceOf[Availability], catalog)
    else
      searchByUserId(criteria.asInstanceOf[UserID], catalog)

/**
 * Represents the possible validation error types in the library.
 */
type LibraryValidationError = String | IllegalArgumentException | NumberFormatException

/**
 * Provides a generic validation result wrapper for computations that may fail.
 */
object ValidationResult:

  /**
   * Represents a successful or failed result.
   * If successful, returns a value of type `T`.
   * If failed, contains a validation error.
   */
  type Result[T] = T | LibraryValidationError

  /**
   * Wraps a successful result.
   *
   * @param value The value to return
   * @return The successful result
   */
  def success[T](value: T): Result[T] = value

  /**
   * Wraps an error message as a failed result.
   *
   * @param message The error message
   * @return The failed result
   */
  def error(message: String): Result[Nothing] = message