package models

import java.time.LocalDateTime

enum TransactionType:
  case Borrow, Return, Reservation
  
final case class Transaction(book: Book, user: User, timestamp: LocalDateTime, transactionType: TransactionType)