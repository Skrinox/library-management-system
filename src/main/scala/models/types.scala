package models

object types:
  opaque type ISBN = String
  object ISBN:
    def apply(value: String): ISBN = value
    extension (isbn: ISBN)
      def value: String = isbn

  opaque type UserID = String
  object UserID:
    def apply(value: String): UserID = value
    extension (id: UserID)
      def value: String = id

