package models

import java.io._

import scala.io.Source

case class Book(title: String, description: String, content: String)
case class Search(keyword: String)

object Book {

  def books: List[Book] = findAll

  def notFound(title: String) = Book("Book Not Found", "", s"Please verify title: $title")

  def findAll = findBooks(new File("books"))

  def findBooks(dir: File): List[Book] = {
    val booksInCurrentDir: List[Book] = dir.listFiles().toList.filter(_.getName.endsWith(".txt")).map(f => {
      val name = f.getName.substring(0, f.getName.indexOf('.'))
      val content = try {
        Source.fromFile(f).getLines().mkString("\n")
      } catch {
        case _: Throwable => ""
      }
      Book(name, if (content == "") "" else content.substring(0, 80) + "...", content)
    }).filter(_.content != "")
    val nestedBooks: List[Book] = dir.listFiles(new FileFilter {
      override def accept(pathname: File): Boolean = pathname.isDirectory
    }).toList.flatMap(subDir => findBooks(subDir))
    booksInCurrentDir ::: nestedBooks
  }


  def findByTitle(title: String): Option[Book] = {
    val filtered = books.filter(_.title == title)
    if (filtered.size > 0) Some(filtered.head) else Some(notFound(title))
  }

  def findByKeyword(s: String): List[Book] = {
    books.filter(_.content.contains(s))
  }
}
