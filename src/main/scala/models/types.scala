package models

object types:
  opaque type ISBN = String
  object ISBN:
    def apply(value: String): ISBN = value
    extension (isbn: ISBN)
      def value: String = isbn
      def isValid: Boolean = isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""")
      def publisher: String = isbn.split("-")(1)

  opaque type UserID = String
  object UserID:
    def apply(value: String): UserID = value
    extension (id: UserID)
      def value: String = id
      def userType: String = 
        if id.startsWith("student-") then "Student"
        else if id.startsWith("faculty-") then "Faculty"
        else if id.startsWith("librarian-") then "Librarian"
        else "Unknown"

