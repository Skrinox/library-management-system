package models

/**
 * Contains domain-specific opaque types used in the library system.
 */
object types:

  /**
   * Represents an International Standard Book Number (ISBN) as an opaque String.
   *
   * Format expected: 978-1-2345-6789-0
   */
  opaque type ISBN = String

  object ISBN:

    /**
     * Creates an ISBN instance from a string value.
     *
     * @param value The raw string to wrap as ISBN
     * @return A new ISBN value
     */
    def apply(value: String): ISBN = value

    /** Extension methods available on ISBN values. */
    extension (isbn: ISBN)

      /** Returns the raw string representation of the ISBN. */
      def value: String = isbn

      /** Validates the ISBN format (must match 13-digit ISBN with hyphens). */
      def isValid: Boolean =
        isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""")

      /** Extracts the publisher code from the ISBN. */
      def publisher: String = isbn.split("-")(1)

  /**
   * Represents a user ID in the system as an opaque String.
   * Prefixes indicate type: "student-", "faculty-", or "librarian-".
   */
  opaque type UserID = String

  object UserID:

    /**
     * Creates a UserID instance from a string.
     *
     * @param value The string representing the user ID
     * @return A new UserID
     */
    def apply(value: String): UserID = value

    /** Extension methods for UserID values. */
    extension (id: UserID)

      /** Returns the raw string representation of the UserID. */
      def value: String = id

      /**
       * Infers the user type from the ID prefix.
       *
       * @return "Student", "Faculty", "Librarian" or "Unknown"
       */
      def userType: String =
        if id.startsWith("student-") then "Student"
        else if id.startsWith("faculty-") then "Faculty"
        else if id.startsWith("librarian-") then "Librarian"
        else "Unknown"