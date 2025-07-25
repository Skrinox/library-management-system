package models
import models.types.*

sealed trait User:
  def id: UserID
  def name: String
  
final case class Student(id: UserID, name: String, email: String, major: String) extends User
final case class Faculty(id: UserID, name: String, department: String) extends User
final case class Librarian(id: UserID, name: String) extends User