package utils

import models.*
import models.Genre

object Extensions:
  extension (isbn: String)
    def isValidISBN: Boolean = isbn.matches("""\d{3}-\d{10}""")

  extension (books: List[models.Book])
    def available: List[models.Book] = books.filter(_.isAvailable)
    def byGenre(genreStr: String): List[models.Book] =
      Genre.values.find(_.toString.equalsIgnoreCase(genreStr)) match
        case Some(g) => books.filter(_.genre == g)
        case None    => List.empty
