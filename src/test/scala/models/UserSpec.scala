package models

import org.scalatest.flatspec.AnyFlatSpec
import models.types.*

class UserSpec extends AnyFlatSpec {

  "A Student" should "be created with valid data" in {
    val userId = UserID("student-001")
    val student = Student(userId, "Alice Smith", "alice@gmail.com", "Data & AI")
  }

  "A Faculty" should "be created with valid data" in {
    val userId = UserID("faculty-001")
    val faculty = Faculty(userId, "Dr. John Doe", "Data & AI")
    assert(faculty.id.value == "faculty-001")
    assert(faculty.name == "Dr. John Doe")
    assert(faculty.department == "Data & AI")
  }

  "A Librarian" should "be created with valid data" in {
    val userId = UserID("librarian-001")
    val librarian = Librarian(userId, "Jane Doe")
    assert(librarian.id.value == "librarian-001")
    assert(librarian.name == "Jane Doe")
  }

}
