package models

import models.types._

/**
 * Base trait for all users of the library system.
 * 
 * All user types must have an ID and a name.
 */
sealed trait User:
  /** The unique identifier of the user. */
  def id: UserID

  /** The full name of the user. */
  def name: String

/**
 * Represents a student user of the library.
 *
 * @param id The unique student ID (usually prefixed with "student-")
 * @param name The full name of the student
 * @param email The student's email address
 * @param major The field of study or major
 */
final case class Student(
  id: UserID,
  name: String,
  email: String,
  major: String
) extends User

/**
 * Represents a faculty member in the library system.
 *
 * @param id The unique faculty ID (usually prefixed with "faculty-")
 * @param name The full name of the faculty member
 * @param department The department they are affiliated with
 */
final case class Faculty(
  id: UserID,
  name: String,
  department: String
) extends User

/**
 * Represents a librarian managing the system.
 *
 * @param id The unique librarian ID (usually prefixed with "librarian-")
 * @param name The name of the librarian
 */
final case class Librarian(
  id: UserID,
  name: String
) extends User