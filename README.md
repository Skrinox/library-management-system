# Library Management System

A simple Scala-based library catalog system allowing users to manage books, users, and transactions via a terminal interface.

[📺 Demo video (5 min)](https://youtu.be/T2J2i9RL9IQ)

## Features

- Add, list, and remove books
- Register users (Student, Faculty, Librarian)
- Borrow and return books
- Track borrowing transactions
- View statistics (book counts, availability, genre distribution)
- Data persistence via JSON with automatic backups

## Project Structure

```
library-management-system/
├── build.sbt
├── data/
│   └── catalog.json
├── src/
│   └── main/
│       └── scala/
│           ├── models/
│           ├── services/
│           └── utils/
└── doc/
```

## How to Run

1. Clone the project.
2. Make sure you have **sbt** and **Java 8+** installed.
3. Run with:

```bash
sbt run
```

4. The program will prompt you through a menu.

## Scaladoc

To generate Scaladoc in the `docs/scaladoc` folder:

```bash
sbt doc
```

## Backup System

Every time you quit the app, `data/catalog.json` is automatically saved and backed up with a timestamp.


_Developed as part of an academic project at EFREI_
