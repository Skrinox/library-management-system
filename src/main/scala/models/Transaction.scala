package models

import java.time.LocalDateTime

/** Represents the type of a transaction (borrow, return, or reservation). */
enum TransactionType:
  case Borrow, Return, Reservation

/**
 * Represents a transaction performed on a book by a user.
 *
 * @param book The book involved in the transaction
 * @param user The user performing the transaction
 * @param timestamp The date and time when the transaction occurred
 * @param transactionType The type of the transaction (borrow, return, reservation)
 */
final case class Transaction(
  book: Book,
  user: User,
  timestamp: LocalDateTime,
  transactionType: TransactionType
)