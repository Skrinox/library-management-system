package services

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import models._
import models.types._
import java.io.File
import java.time.LocalDateTime

class FileIOSpec extends AnyFunSuite with BeforeAndAfterEach {

  val testFile = "test-catalog.json"
  val testBook: Book = Book(
    ISBN("978-0-123456-78-9"),
    "Test Book",
    List("Test Author"),
    2020,
    Genre.Fiction,
    Availability.Available
  )
  
  val testUser: Student = Student(
    UserID("student-001"),
    "John Doe",
    "john@example.com",
    "Computer Science"
  )
  
  val testTransaction: Transaction = Transaction(
    testBook,
    testUser,
    LocalDateTime.now(),
    TransactionType.Borrow
  )
  
  val testCatalog: LibraryCatalog = LibraryCatalog(List(testBook), List(testUser), List(testTransaction))

  override def afterEach(): Unit = {
    // Nettoyer les fichiers de test
    val file = new File(testFile)
    if (file.exists()) file.delete()
    
    // Nettoyer les fichiers de backup
    val backupFiles = new File(".").listFiles().filter(_.getName.startsWith(testFile + ".backup"))
    backupFiles.foreach(_.delete())
  }

  test("saveCatalogToFile creates file") {
    val result = FileIO.saveCatalogToFile(testCatalog, testFile)
    assert(result.isRight)
    assert(new File(testFile).exists())
  }

  test("saveBooksToFile creates JSON file") {
    val result = FileIO.saveBooksToFile(List(testBook), testFile)
    assert(result.isRight)
    
    val content = FileIO.loadFileContent(testFile)
    assert(content.isRight)
    content.foreach { json =>
      assert(json.contains("Test Book"))
      assert(json.contains("978-0-123456-78-9"))
    }
  }

  test("saveUsersToFile creates JSON file") {
    val result = FileIO.saveUsersToFile(List(testUser), testFile)
    assert(result.isRight)
    
    val content = FileIO.loadFileContent(testFile)
    assert(content.isRight)
    content.foreach { json =>
      assert(json.contains("John Doe"))
      assert(json.contains("Student"))
    }
  }

  test("loadFileContent with non-existing file") {
    val result = FileIO.loadFileContent("non-existing-file.json")
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[IOError])
    }
  }

  test("JSON formatting for book") {
    val json = FileIO.JsonFormats.bookToJson(testBook)
    assert(json.contains("\"isbn\": \"978-0-123456-78-9\""))
    assert(json.contains("\"title\": \"Test Book\""))
    assert(json.contains("\"authors\": [\"Test Author\"]"))
    assert(json.contains("\"publicationYear\": 2020"))
  }

  test("JSON formatting for user") {
    val json = FileIO.JsonFormats.userToJson(testUser)
    assert(json.contains("\"type\": \"Student\""))
    assert(json.contains("\"name\": \"John Doe\""))
    assert(json.contains("\"email\": \"john@example.com\""))
    assert(json.contains("\"major\": \"Computer Science\""))
  }

  test("exportCatalogReport generates report") {
    val reportFile = "test-report.txt"
    val result = FileIO.exportCatalogReport(testCatalog, reportFile)
    assert(result.isRight)
    
    val content = FileIO.loadFileContent(reportFile)
    assert(content.isRight)
    content.foreach { report =>
      assert(report.contains("Library Catalog Report"))
      assert(report.contains("Books: 1"))
      assert(report.contains("Users: 1"))
      assert(report.contains("Transactions: 1"))
    }
    
    // Nettoyer
    new File(reportFile).delete()
  }

  test("saveWithBackup creates backup") {
    // Créer un fichier initial
    FileIO.saveCatalogToFile(testCatalog, testFile)
    
    // Sauvegarder avec backup
    val result = FileIO.saveWithBackup(testCatalog, testFile)
    assert(result.isRight)
    
    // Vérifier qu'un fichier de backup a été créé
    val backupFiles = new File(".").listFiles().filter(_.getName.startsWith(testFile + ".backup"))
    assert(backupFiles.nonEmpty)
  }

  test("escapeJson handles special characters") {
    val bookWithSpecialChars = testBook.copy(title = "Test \"Book\" with\nnewlines")
    val json = FileIO.JsonFormats.bookToJson(bookWithSpecialChars)
    assert(json.contains("Test \\\"Book\\\" with\\nnewlines"))
  }
}