package controllers

import javax.inject._
import models.{Book, Search}
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.mvc.Flash

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Redirect(routes.HomeController.list())
  }

  def list = Action { implicit request =>
    val books = Book.books
    Ok(views.html.list(books))
  }

  def show(title: String) = Action { implicit request =>
    Book.findByTitle(title).map { b =>
      Ok(views.html.content(b))
    }.getOrElse(NotFound)
  }

  def showError(s: String) = Action { implicit request =>
    Ok(views.html.content(Book("Book Not Found", "Please verify keyword: " + s, "")))
  }

  private val searchForm: Form[Search] = Form(
    mapping(
      "keyword" -> of[String]
    )(Search.apply)(Search.unapply)
  )

  def results = Action { implicit request: MessagesRequest[AnyContent] =>
    val newSearchForm = searchForm.bindFromRequest()
    newSearchForm.fold(
      hasErrors = {s =>
        val books = List()
        Ok(views.html.list(books)).flashing(Flash(s.data) + ("error" -> "error occurred"))
      },
      success = {s =>
        val books = Book.findByKeyword(s.keyword)
        Ok(views.html.list(books)).flashing("success" -> "good job!")
      }
    )
  }

  def search = Action { implicit request: MessagesRequest[AnyContent] =>
    val form =
      if (request.flash.get("error").isDefined)
        searchForm.bind(request.flash.data)
      else
        searchForm
    Ok(views.html.search(form))
  }


}
