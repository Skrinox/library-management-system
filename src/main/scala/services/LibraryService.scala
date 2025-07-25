package services

import models._
import DomainTypes._

object LibraryService:
  def searchBookByISBN(isbn: ISBN, catalog: List[Book]): Book | String =
    catalog.find(_.isbn == isbn) match
      case Some(book) => book
      case None       => s"No book found with ISBN: ${isbn.value}"