package models

enum UserType:
  case Student, Faculty, Librarian

enum BookStatus:
  case Available, CheckedOut, Reserved