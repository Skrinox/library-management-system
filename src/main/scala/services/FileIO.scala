package services

import models._
import models.types._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import java.io.{PrintWriter, File}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object FileIO:

  /**
   * Formats for converting models to JSON strings.
   */
  object JsonFormats:

    /**
     * Converts a Book to a JSON string.
     * @param book the book to convert
     * @return
     */
    def bookToJson(book: Book): String =
      s"""{
         |  "isbn": "${book.isbn.value}",
         |  "title": "${escapeJson(book.title)}",
         |  "authors": [${book.authors.map(a => s""""${escapeJson(a)}"""").mkString(", ")}],
         |  "publicationYear": ${book.publicationYear},
         |  "genre": "${book.genre}",
         |  "availability": "${book.availability}"
         |}""".stripMargin

    /**
     * Converts a User to a JSON string.
     * @param user the user to convert
     * @return
     */
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

    /**
     * Converts a Transaction to a JSON string.
     * @param transaction the transaction to convert
     * @return
     */
    private def transactionToJson(transaction: Transaction): String =
      s"""{
         |  "book": ${bookToJson(transaction.book)},
         |  "user": ${userToJson(transaction.user)},
         |  "timestamp": "${transaction.timestamp.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}",
         |  "transactionType": "${transaction.transactionType}"
         |}""".stripMargin

    /**
     * Converts a LibraryCatalog to a JSON string.
     * @param catalog the catalog to convert
     * @return
     */
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

    /**
     * Escapes special characters in a JSON string.
     * @param str the string to escape
     * @return
     */
    private def escapeJson(str: String): String =
      str.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")

  /**
   * Saves the library catalog to a file in JSON format.
   * @param catalog the library catalog to save
   * @param filename the name of the file to save to
   * @return
   */
  def saveCatalogToFile(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = JsonFormats.catalogToJson(catalog)
      val writer = new PrintWriter(new File(filename))
      try writer.write(json)
      finally writer.close()
    }

  /**
   * Saves a list of books to a file in JSON format.
   * @param books the list of books to save
   * @param filename the name of the file to save to
   * @return
   */
  def saveBooksToFile(books: List[Book], filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = s"""[
                    |  ${books.map(JsonFormats.bookToJson).mkString(",\n  ")}
                    |]""".stripMargin
      val writer = new PrintWriter(new File(filename))
      try writer.write(json)
      finally writer.close()
    }

  /**
   * Saves a list of users to a file in JSON format.
   * @param users the list of users to save
   * @param filename the name of the file to save to
   * @return
   */
  def saveUsersToFile(users: List[User], filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val json = s"""[
                    |  ${users.map(JsonFormats.userToJson).mkString(",\n  ")}
                    |]""".stripMargin
      val writer = new PrintWriter(new File(filename))
      try writer.write(json)
      finally writer.close()
    }

  /**
   * Loads the content of a file as a string.
   * @param filename the name of the file to load
   * @return
   */
  def loadFileContent(filename: String): LibraryResult[String] =
    ErrorHandling.tryToEither {
      val source = Source.fromFile(filename)
      try source.mkString
      finally source.close()
    }

  /**
   * Parses a Book from a JSON string.
   * @param jsonStr the JSON string representing a book
   * @return a LibraryResult containing the parsed Book or an error
   */
  private def parseBookFromJson(jsonStr: String): LibraryResult[Book] =
    ErrorHandling.tryToEither {
      val isbn = extractJsonValue(jsonStr, "isbn")
      val title = extractJsonValue(jsonStr, "title")
      val authors = extractJsonArray(jsonStr, "authors") match
        case a if a.nonEmpty =>
          a.split(",")
            .map(_.trim.stripPrefix("\"").stripSuffix("\""))
            .filter(_.nonEmpty)
            .toList
        case _ => Nil
      val year = extractJsonValue(jsonStr, "publicationYear").toInt
      val genreStr = extractJsonValue(jsonStr, "genre")
      val availabilityStr = extractJsonValue(jsonStr, "availability")
      val genre = Genre.valueOf(genreStr)
      val availability = Availability.valueOf(availabilityStr)
      Book(ISBN(isbn), title, authors, year, genre, availability)
    }.left.map(error => ParseError("Failed to parse book from JSON", jsonStr))

  /**
   * Saves the library catalog to a file with a backup.
   * @param catalog the library catalog to save
   * @param filename the name of the file to save to
   * @return
   */
  def saveWithBackup(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    for
      _ <- createBackup(filename)
      _ <- saveCatalogToFile(catalog, filename)
    yield ()

  /**
   * Creates a backup of the specified file by copying its content to a new file with a timestamp.
   * @param filename the name of the file to back up
   * @return
   */
  private def createBackup(filename: String): LibraryResult[Unit] =
    val backupName = s"$filename.backup.${System.currentTimeMillis()}"
    ErrorHandling.tryToEither {
      val originalFile = new File(filename)
      if originalFile.exists() then
        val source = Source.fromFile(originalFile)
        val content = try source.mkString finally source.close()
        val writer = new PrintWriter(new File(backupName))
        try writer.write(content)
        finally writer.close()
    }

  /**
   * Extracts a value from a JSON string by key.
   * @param json the JSON string to search
   * @param key the key to extract the value for
   * @return
   */
  private def extractJsonValue(json: String, key: String): String =
    val patternQuoted = s""""$key"\\s*:\\s*"([^"]*)"""".r
    val patternUnquoted = s""""$key"\\s*:\\s*([^",}\\s]+)""".r

    patternQuoted.findFirstMatchIn(json)
      .map(_.group(1))
      .orElse(patternUnquoted.findFirstMatchIn(json).map(_.group(1)))
      .getOrElse(throw new IllegalArgumentException(s"Key '$key' not found in JSON"))

  /**
   * Extracts a JSON array from a string by key.
   * @param json the JSON string to search
   * @param key the key to extract the array for
   * @return
   */
  private def extractJsonArray(json: String, key: String): String =
    val pattern = s"""\"$key\"\\s*:\\s*\\[([^]]*)]""".r
    pattern.findFirstMatchIn(json) match
      case Some(m) => m.group(1)
      case None => throw new IllegalArgumentException(s"Array '$key' not found in JSON")

  /**
   * Exports a report of the library catalog to a file.
   * @param catalog the library catalog to generate the report for
   * @param filename the name of the file to save the report to
   * @return
   */
  def exportCatalogReport(catalog: LibraryCatalog, filename: String): LibraryResult[Unit] =
    ErrorHandling.tryToEither {
      val report = generateCatalogReport(catalog)
      val writer = new PrintWriter(new File(filename))
      try writer.write(report)
      finally writer.close()
    }

  /**
   * Generates a report string summarizing the library catalog.
   * @param catalog the library catalog to summarize
   * @return
   */
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

  /**
   * Loads a library catalog from a JSON file.
   * @param filename the name of the file to load
   * @return
   */
  def loadCatalogFromFile(filename: String): Option[LibraryCatalog] =
    loadFileContent(filename) match
      case Right(jsonStr) =>
        try
          val booksJson = extractArray(jsonStr, "books")
          val usersJson = extractArray(jsonStr, "users")
          val txJson = extractArray(jsonStr, "transactions")
          val books = booksJson.flatMap(parseBookFromJson(_).toOption)
          val users = usersJson.flatMap(parseUserFromJson(_).toOption)
          val transactions = txJson.flatMap(parseTransactionFromJson(_).toOption)

          println(s"Parsed ${books.length} books, ${users.length} users, ${transactions.length} transactions")
          Some(LibraryCatalog(books, users, transactions))
        catch case e: Throwable =>
          println(s"Error while parsing: ${e.getMessage}")
          None

      case Left(err) =>
        println(s"Failed to read file $filename: ${err.message}")
        None

  /**
   * Extracts an array of JSON objects from a JSON string by key.
   * @param json the JSON string to search
   * @param key the key to extract the array for
   * @return
   */
  private def extractArray(json: String, key: String): List[String] = {
    val idxKey = json.indexOf(s""""$key"""")
    if (idxKey < 0) return Nil
    val idxBracket = json.indexOf('[', idxKey)
    if (idxBracket < 0) return Nil

    var depth = 0
    var endIdx = -1
    for (i <- idxBracket until json.length if endIdx < 0) {
      json(i) match {
        case '[' => depth += 1
        case ']' =>
          depth -= 1
          if (depth == 0) endIdx = i
        case _ => ()
      }
    }
    if (endIdx < 0) return Nil

    val raw = json.substring(idxBracket + 1, endIdx)

    val elems = scala.collection.mutable.ListBuffer.empty[String]
    val sb    = new StringBuilder
    depth = 0
    var inString = false
    var escape   = false

    raw.foreach {
      case '\\' =>
        escape = !escape
        sb.append('\\')

      case '"' if !escape =>
        inString = !inString
        sb.append('"')
        escape = false

      case '"' if escape =>
        sb.append('"')
        escape = false

      case '{' if !inString =>
        depth += 1
        sb.append('{')

      case '}' if !inString =>
        depth -= 1
        sb.append('}')
        if depth == 0 then
          elems += sb.toString()
          sb.clear()

      case c =>
        sb.append(c)
        escape = false
    }

    elems.toList.map(_.trim).filter(_.nonEmpty)
  }

  /**
   * Parses a User from a JSON string.
   * @param jsonStr the JSON string representing a user
   * @return a LibraryResult containing the parsed User or an error
   */
  private def parseUserFromJson(jsonStr: String): LibraryResult[User] =
    ErrorHandling.tryToEither {
      val id = extractJsonValue(jsonStr, "id")
      val name = extractJsonValue(jsonStr, "name")
      val userType = extractJsonValue(jsonStr, "type")
      userType match
        case "Student" =>
          val email = extractJsonValue(jsonStr, "email")
          val major = extractJsonValue(jsonStr, "major")
          Student(UserID(id), name, email, major)
        case "Faculty" =>
          val department = extractJsonValue(jsonStr, "department")
          Faculty(UserID(id), name, department)
        case "Librarian" =>
          Librarian(UserID(id), name)
        case other => throw new IllegalArgumentException(s"Unknown user type: $other")
    }.left.map(error => ParseError("Failed to parse user", jsonStr))

  /**
   * Parses a Transaction from a JSON string.
   * @param jsonStr the JSON string representing a transaction
   * @return a LibraryResult containing the parsed Transaction or an error
   */
  private def parseTransactionFromJson(jsonStr: String): LibraryResult[Transaction] =
    ErrorHandling.tryToEither {
      val bookJson = extractNestedJson(jsonStr, "book")
      val userJson = extractNestedJson(jsonStr, "user")
      val timestamp = extractJsonValue(jsonStr, "timestamp")
      val transactionTypeStr = extractJsonValue(jsonStr, "transactionType")
      val book = parseBookFromJson(bookJson).getOrElse(throw new RuntimeException("Invalid book in transaction"))
      val user = parseUserFromJson(userJson).getOrElse(throw new RuntimeException("Invalid user in transaction"))
      val time = LocalDateTime.parse(timestamp)
      val transactionType = TransactionType.valueOf(transactionTypeStr)
      Transaction(book, user, time, transactionType)
    }.left.map(error => ParseError("Failed to parse transaction", jsonStr))

  /**
   * Extracts a nested JSON object from a JSON string by key.
   * @param json the JSON string to search
   * @param key the key to extract the nested object for
   * @return the nested JSON object as a string
   */
  private def extractNestedJson(json: String, key: String): String =
    val pattern = s"""\"$key\"\\s*:\\s*\\{(.*?)}""".r
    pattern.findFirstMatchIn(json) match
      case Some(m) => "{" + m.group(1) + "}"
      case None => throw new IllegalArgumentException(s"Nested object '$key' not found")