package models

object DomainTypes:
  opaque type ISBN = String
  opaque type UserID = String

  object ISBN:
    def apply(raw: String): ISBN = raw
    extension (isbn: ISBN)
      def value: String = isbn
      def isValid: Boolean = isbn.matches("""\d{3}-\d{10}""")

  object UserID:
    def apply(id: String): UserID = id
    extension (uid: UserID)
      def value: String = uid