package services

import models._
import models.types._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import java.io.{PrintWriter, File}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object FileIO:
  
  // Formats JSON simples (sans bibliothèque externe)
  object JsonFormats:
    
    def bookToJson(book: Book): String =
      s"""{
        |  "isbn": "${book.isbn.value}",
        |  "title": "${escapeJson(book.title)}",
        |  "authors": [${book.authors.map(a => s""""${escapeJson(a)}"""").mkString(", ")}],
        |  "publicationYear": ${book.publicationYear},
        |  "genre": "${book.genre}",
        |  "availability": "${book.availability}"
        |}""".stripMargin
    
    def userToJson(user: User): String = user match
      case Student(id, name, email, major) =>
        s"""{
          |  "type": "Student",
          |  "id": "${id.value}",
          |  "name": "${escapeJson(name)}",
          |  "email": "${escapeJson(email)}",
          |  "major": "${escapeJson(major)}"
          |}""".stripMargin
      case Faculty(id, name, department) =>
        s"""{
          |  "type": "Faculty",
          |  "id": "${id.value}",
          |  "name": "${escapeJson(name)}",
          |  "department": "${escapeJson(department)}"
          |}""".stripMargin
      case Librarian(id, name) =>
        s"""{
          |  "type": "Librarian",
          |  "id": "${id.value}",
          |  "name": "${escapeJson(name)}"
          |}""".stripMargin
    
    def transactionToJson(transaction: Transaction): String =
      s"""{
        |  "book": ${bookToJson(transaction.book)},
        |  "user": ${userToJson(transaction.user)},
        |  "timestamp": "${transaction.timestamp.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}",
        |  "transactionType": "${transaction.transactionType}"
        |}""".stripMargin
    
    def catalogToJson(catalog: LibraryCatalog): String =
      s"""{
        |  "books": [
        |    ${catalog.books.map(bookToJson).mkString(",\n    ")}
        |  ],
        |  "users": [
        |    ${catalog.users.map(userToJson).mkString(",\n    ")}
        |  ],
        |  "transactions": [
        |    ${catalog.transactions.map(transactionToJson).mkString(",\n    ")}
        |  ]
        |}""".stripMargin
    
    private def escapeJson(str: String): String =
      str.replace("\\", "\\\\")
         .replace("\"", "\\\"")
         .replace("\n", "\\n")
         .replace("\r", "\\r")
         .replace("\t", "\\t")
  
  // Sauvegarde des données
  def saveCatalogToFile(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = JsonFormats.catalogToJson(catalog)
      val writer = new PrintWriter(new File(filename))
      try
        writer.write(json)
      finally
        writer.close()
    }
  
  def saveBooksToFile(books: List[Book], filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = s"""[
        |  ${books.map(JsonFormats.bookToJson).mkString(",\n  ")}
        |]""".stripMargin
      val writer = new PrintWriter(new File(filename))
      try
        writer.write(json)
      finally
        writer.close()
    }
  
  def saveUsersToFile(users: List[User], filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = s"""[
        |  ${users.map(JsonFormats.userToJson).mkString(",\n  ")}
        |]""".stripMargin
      val writer = new PrintWriter(new File(filename))
      try
        writer.write(json)
      finally
        writer.close()
    }
  
  // Chargement des données (parsing JSON simple)
  def loadFileContent(filename: String): LibraryResult[String] =
    ErrorHandling.tryToEither {
      val source = Source.fromFile(filename)
      try
        source.mkString
      finally
        source.close()
    }
  
  // Parsing simple des livres depuis JSON
  def parseBookFromJson(jsonStr: String): LibraryResult[Book] =
    ErrorHandling.tryToEither {
      // Parsing très basique - dans un vrai projet, utilisez une bibliothèque JSON
      val isbn = extractJsonValue(jsonStr, "isbn")
      val title = extractJsonValue(jsonStr, "title")
      val authorsStr = extractJsonArray(jsonStr, "authors")
      val authors = authorsStr.split(",").map(_.trim.stripPrefix("\"").stripSuffix("\"")).toList
      val year = extractJsonValue(jsonStr, "publicationYear").toInt
      val genreStr = extractJsonValue(jsonStr, "genre")
      val availabilityStr = extractJsonValue(jsonStr, "availability")
      
      val genre = Genre.valueOf(genreStr)
      val availability = Availability.valueOf(availabilityStr)
      
      Book(ISBN(isbn), title, authors, year, genre, availability)
    }.left.map(error => ParseError(s"Failed to parse book from JSON", jsonStr))
  
  // Sauvegarde avec gestion d'erreurs et backup
  def saveWithBackup(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    for
      _ <- createBackup(filename)
      _ <- saveCatalogToFile(catalog, filename)
    yield ()
  
  def createBackup(filename: String): LibraryResult[Unit] =
    val backupName = s"$filename.backup.${System.currentTimeMillis()}"
    ErrorHandling.tryToEither {
      val originalFile = new File(filename)
      if originalFile.exists() then
        val source = Source.fromFile(originalFile)
        val content = try source.mkString finally source.close()
        val writer = new PrintWriter(new File(backupName))
        try writer.write(content) finally writer.close()
    }
  
  // Utilitaires de parsing JSON basique
  private def extractJsonValue(json: String, key: String): String =
    val pattern = s""""$key"\\s*:\\s*"([^"]*)")""".r
    pattern.findFirstMatchIn(json) match
      case Some(m) => m.group(1)
      case None => throw new IllegalArgumentException(s"Key '$key' not found in JSON")
  
  private def extractJsonArray(json: String, key: String): String =
    val pattern = s""""$key"\\s*:\\s*\\[([^\\]]*)\\]""".r
    pattern.findFirstMatchIn(json) match
      case Some(m) => m.group(1)
      case None => throw new IllegalArgumentException(s"Array '$key' not found in JSON")
  
  // Opérations avancées avec gestion d'erreurs
  def exportCatalogReport(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val report = generateCatalogReport(catalog)
      val writer = new PrintWriter(new File(filename))
      try
        writer.write(report)
      finally
        writer.close()
    }
  
  private def generateCatalogReport(catalog: LibraryCatalog): String =
    s"""Library Catalog Report
       |Generated on: ${LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}
       |
       |Books: ${catalog.books.length}
       |Users: ${catalog.users.length}
       |Transactions: ${catalog.transactions.length}
       |
       |Available Books: ${catalog.books.count(_.isAvailable)}
       |Checked Out Books: ${catalog.books.count(_.availability == Availability.CheckedOut)}
       |
       |Genres Distribution:
       |${catalog.books.groupBy(_.genre).map { case (genre, books) => s"  $genre: ${books.length}" }.mkString("\n")}
       |
       |Users by Type:
       |${catalog.users.groupBy(_.getClass.getSimpleName).map { case (userType, users) => s"  $userType: ${users.length}" }.mkString("\n")}
       |""".stripMargin