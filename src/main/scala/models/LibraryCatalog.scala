package models
import models.types.*

final case class LibraryCatalog(
  books: List[Book],
  users: List[User],
  transactions: List[Transaction]
                               ):
  def addBook(book: Book): LibraryCatalog = copy(books = book :: books)
  def addUser(user: User): LibraryCatalog = copy(users = user :: users)
  def addTransaction(transaction: Transaction): LibraryCatalog = copy(transactions = transaction :: transactions)
  
  def removeBook(isbn: ISBN): LibraryCatalog = copy(books = books.filterNot(_.isbn == isbn))
  def removeUser(userId: UserID): LibraryCatalog = copy(users = users.filterNot(_.id == userId))
  def removeTransaction(transaction: Transaction): LibraryCatalog = copy(transactions = transactions.filterNot(_ == transaction))
  
