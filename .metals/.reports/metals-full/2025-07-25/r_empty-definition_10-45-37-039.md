error id: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Validation.scala:`<none>`.
file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Validation.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -models/email.
	 -email.
	 -scala/Predef.email.
offset: 748
uri: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Validation.scala
text:
```scala
package services

import models._

object Validation:

  // 🔍 Validation d’un livre : ISBN non vide, au moins 1 auteur, année positive
  def validateBook(book: Book): Either[String, Book] =
    if book.isbn.value.trim.isEmpty then Left("ISBN ne peut pas être vide")
    else if book.authors.isEmpty then Left("Le livre doit avoir au moins un auteur")
    else if book.publicationYear <= 0 then Left("Année de publication invalide")
    else Right(book)

  // 👤 Validation d’un utilisateur : nom non vide + champ spécifique
  def validateUser(user: User): Either[String, User] = user match
    case Student(id, name, email, major) =>
      if name.trim.isEmpty then Left("Nom étudiant vide")
      else if email.trim.isEmpty || !e@@mail.contains("@") then Left("Email étudiant invalide")
      else if major.trim.isEmpty then Left("Filière non précisée")
      else Right(user)

    case Faculty(id, name, department) =>
      if name.trim.isEmpty then Left("Nom enseignant vide")
      else if department.trim.isEmpty then Left("Département manquant")
      else Right(user)

    case Librarian(id, name) =>
      if name.trim.isEmpty then Left("Nom bibliothécaire vide")
      else Right(user)

  // 🔁 Validation de transaction : livre dispo + utilisateur valide
  def validateTransaction(book: Book, user: User): Either[String, (Book, User)] =
    for
      b <- validateBook(book)
      u <- validateUser(user)
      _ <- if b.isAvailable then Right(()) else Left("Le livre n'est pas disponible")
    yield (b, u)

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.