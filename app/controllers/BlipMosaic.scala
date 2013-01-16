package controllers
import play.api.mvc.Action
import play.api.mvc.Controller

object BlipMosaic extends Controller {

  def index = Action {
    Ok("BOOYA")
  }
}