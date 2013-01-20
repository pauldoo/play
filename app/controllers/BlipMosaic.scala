package controllers
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise
import scala.xml.Node

object BlipMosaic extends Controller {

  def index = Action {
    Ok("BOOYA")
  }

  def generate(user: String) = Action {
    Async {
      val baseUrl: String = "http://api.blipfoto.com/get/search/";
      val promise: Promise[Response] = WS.url(baseUrl).withQueryString(
        ("api_key", "617a7d96081cde9e7207509a96b516a0"),
        ("version", "2"),
        ("format", "XML"),
        ("query", "by " + user),
        ("size", "small")).get();

      promise.map { response =>
        val body : Node = response.xml;
        val k = (body \\ "thumbnail").text;
        Ok(k);
      }
    }
  }
}