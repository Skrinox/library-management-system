package services

import models._

/**
 * Provides functional search and filtering utilities for books
 */
object Search:

  /**
   * Filters a list of books using a given predicate
   *
   * @param books List of books
   * @param predicate A condition to test each book
   * @return List of books that satisfy the predicate
   */
  def filterBooks(books: List[Book])(predicate: Book => Boolean): List[Book] =
    books.filter(predicate)

  /**
   * Creates a predicate to filter books by title (case-insensitive)
   *
   * @param title Partial or full title to search
   * @return A function that checks if a book title contains the input
   */
  def byTitle(title: String): Book => Boolean =
    _.title.toLowerCase.contains(title.toLowerCase)

  /**
   * Creates a predicate to filter books by author name (case-insensitive)
   *
   * @param author Partial or full author name
   * @return A function that checks if any author matches the input
   */
  def byAuthor(author: String): Book => Boolean =
    _.authors.exists(_.toLowerCase.contains(author.toLowerCase))

  /**
   * Creates a predicate to filter books by genre
   *
   * @param genre The genre to match
   * @return A function that checks if a book matches the genre
   */
  def byGenre(genre: Genre): Book => Boolean =
    _.genre == genre

  /**
   * Creates a predicate to filter books by availability status
   *
   * @param status The availability status to match
   * @return A function that checks if a book has the specified status
   */
  def byAvailability(status: Availability): Book => Boolean =
    _.availability == status

  /**
   * Composes two predicates with logical AND
   *
   * @param f1 First predicate
   * @param f2 Second predicate
   * @return A predicate that is true if both f1 and f2 are true
   */
  private def composeFilters(f1: Book => Boolean, f2: Book => Boolean): Book => Boolean =
    book => f1(book) && f2(book)

  /**
   * Searches for books that match both a title and a genre
   *
   * @param books List of books
   * @param title Title query string
   * @param genre Genre to match
   * @return List of books matching both criteria
   */
  def searchByTitleAndGenre(books: List[Book], title: String, genre: Genre): List[Book] =
    val combined = composeFilters(byTitle(title), byGenre(genre))
    filterBooks(books)(combined)