package models

import models.types.*

/**
 * Represents the main library catalog, storing books, users, and transactions.
 *
 * @param books List of all books in the system
 * @param users List of all registered users
 * @param transactions History of all borrow and return operations
 */
final case class LibraryCatalog(
  books: List[Book],
  users: List[User],
  transactions: List[Transaction]
):

  /**
   * Adds a new book to the catalog.
   *
   * @param book The book to add
   * @return A new LibraryCatalog instance with the added book
   */
  def addBook(book: Book): LibraryCatalog =
    copy(books = book :: books)

  /**
   * Adds a new user to the catalog.
   *
   * @param user The user to add
   * @return A new LibraryCatalog instance with the added user
   */
  def addUser(user: User): LibraryCatalog =
    copy(users = user :: users)

  /**
   * Records a transaction (borrow or return).
   *
   * @param transaction The transaction to add
   * @return A new LibraryCatalog instance with the added transaction
   */
  def addTransaction(transaction: Transaction): LibraryCatalog =
    copy(transactions = transaction :: transactions)

  /**
   * Removes a book from the catalog by ISBN.
   *
   * @param isbn The ISBN of the book to remove
   * @return A new LibraryCatalog instance without the specified book
   */
  def removeBook(isbn: ISBN): LibraryCatalog =
    copy(books = books.filterNot(_.isbn == isbn))

  /**
   * Removes a user from the catalog by user ID.
   *
   * @param userId The ID of the user to remove
   * @return A new LibraryCatalog instance without the specified user
   */
  def removeUser(userId: UserID): LibraryCatalog =
    copy(users = users.filterNot(_.id == userId))

  /**
   * Removes a transaction from the catalog.
   *
   * @param transaction The transaction to remove
   * @return A new LibraryCatalog instance without the specified transaction
   */
  def removeTransaction(transaction: Transaction): LibraryCatalog = copy(transactions = transactions.filterNot(_ == transaction))