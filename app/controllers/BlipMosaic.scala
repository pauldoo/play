package controllers
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise
import scala.xml.Node
import play.api.libs.concurrent.Akka
import akka.dispatch.Future
import play.api.Play.current
import play.api.cache.Cache
import java.awt.Image
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.net.URL
import play.api.Logger
import java.io.ByteArrayInputStream
import java.io.FileOutputStream
import play.libs.Akka.asPromise

object BlipMosaic extends Controller {
  val featureWidth = 3;
  val featureHeight = 3;
    val tileSize = 16;

  def index = Action {
    Ok("BOOYA")
  }

  class ThumbnailWithFeatures(
    val entryId: String,
    val thumbnailUrl: String,
    val features: IndexedSeq[Double]) {

  }

  def generate(searchQuery: String, targetEntryId: String) = Action {
    Async {

      val baseUrl: String = "http://api.blipfoto.com/get/search/";
      val searchPromise: Promise[Response] = WS.url(baseUrl).withQueryString(
        ("api_key", "617a7d96081cde9e7207509a96b516a0"),
        ("version", "2"),
        ("format", "XML"),
        ("query", searchQuery),
        ("size", "small"),
        ("max", "90")).get();

      val thumbFeatures: Promise[Seq[ThumbnailWithFeatures]] = searchPromise.flatMap { response =>
        val body: Node = response.xml;
        val thumbnailPromises: Seq[Promise[ThumbnailWithFeatures]] = for {
          item <- (body \\ "item");
          entryId = (item \ "entry_id").text;
          thumbnailUrl = (item \ "thumbnail").text
        } yield (
          getThumbnailFeatures(thumbnailUrl).map {
            k => new ThumbnailWithFeatures(entryId, thumbnailUrl, k)
          });
        Promise.sequence(thumbnailPromises);
      }

      val entryDetailsUrl: String = "http://api.blipfoto.com/get/entry/";
      val targetImage: Promise[BufferedImage] = WS.url(entryDetailsUrl).withQueryString(
        ("api_key", "617a7d96081cde9e7207509a96b516a0"),
        ("version", "2"),
        ("format", "XML"),
        ("entry_id", targetEntryId),
        ("data", "image,large_image")).get().flatMap { response =>
          val body: Node = response.xml;
          val normalUrl = (body \\ "image").text;
          val largeUrl = (body \\ "large_image").text;
          val imageUrl: String = if (largeUrl.isEmpty()) normalUrl else largeUrl;
          Logger.info("Target image #" + targetEntryId + " found: " + imageUrl);
          getImage(imageUrl);
        }

      val result: Promise[Tuple2[Seq[ThumbnailWithFeatures], BufferedImage]] = for {
        thumbs <- thumbFeatures;
        target <- targetImage
      } yield (thumbs, target);

      result.map { tuple =>
        val chosenThumbnails: Seq[Seq[String]] = chooseThumbnails(tuple._1, tuple._2);
        Ok(views.html.BlipMosaic.tableMosaic(chosenThumbnails, tileSize))
      }
    }
  }

  def chooseThumbnails(thumbnails: Seq[ThumbnailWithFeatures], target: BufferedImage): Seq[Seq[String]] = {
    val columns: Int = target.getWidth() / tileSize;
    val rows: Int = target.getHeight() / tileSize;
    val scaledTarget = toScaledBufferedImage(target, columns * featureWidth, rows * featureHeight);

    val targetFeatures: Seq[Seq[Seq[Double]]] =
      for (r <- 0 until rows)
        yield for (c <- 0 until columns) yield {
        extractFeatures(scaledTarget.getSubimage(c * featureWidth, r * featureHeight, featureWidth, featureHeight));
      };

    targetFeatures.map { _.map { fragment => closestThumbnailTo(fragment, thumbnails).thumbnailUrl } };
  }

  def closestThumbnailTo(
    targetFragment: Seq[Double],
    thumbnails: Seq[ThumbnailWithFeatures]
    ): ThumbnailWithFeatures = {
    thumbnails.minBy{t => rmsDifference(t.features, targetFragment)};
  }

  def rmsDifference(a: Seq[Double], b: Seq[Double]): Double = {
    assert(a.length == b.length);
    val square : Double => Double = {k=>k*k};
    math.sqrt((a zip b).map { t => square(t._1 - t._2) }.reduce(_ + _) / a.length);
  }

  def getThumbnailFeatures(thumbnailUrl: String): Promise[IndexedSeq[Double]] = {
    val key = thumbnailUrl + ".features";
    Cache.getOrElse[Promise[IndexedSeq[Double]]](key) {
      Logger.info("Calculating features for image: " + thumbnailUrl);
      val imagePromise: Promise[BufferedImage] = getImage(thumbnailUrl);
      imagePromise.map(image => {
        val scaledImage: BufferedImage = toScaledBufferedImage(image, featureWidth, featureHeight);

        val featureValues: IndexedSeq[Double] = extractFeatures(scaledImage);
        Logger.info("Computed feature values: " + featureValues + ", for image: " + thumbnailUrl);
        featureValues;
      });
    }
  }

  def extractFeatures(scaledImage: BufferedImage): IndexedSeq[Double] = {
    assert(scaledImage.getWidth() == featureWidth);
    assert(scaledImage.getHeight() == featureHeight);
    for {
      x <- 0 until featureWidth;
      y <- 0 until featureHeight;
      c <- 0 until 3
    } yield ((scaledImage.getRGB(x, y) >> (c * 8)) & (0xFF)) / 255.0;
  }

  def toScaledBufferedImage(source: Image, width: Int, height: Int): BufferedImage = {
    val scaled = source.getScaledInstance(width, height, Image.SCALE_SMOOTH);
    val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    val g = result.getGraphics();
    g.drawImage(scaled, 0, 0, null);
    g.dispose();
    return result;
  }

  def getImage(imageUrl: String): Promise[BufferedImage] = {
    // TODO: Should really cache the raw bytes, not the decompressed image
    val key = imageUrl + ".image";
    Cache.getOrElse[Promise[BufferedImage]](key) {
      Logger.info("Downloading image: " + imageUrl);
      Akka.future {
        ImageIO.read(new URL(imageUrl));
      }
    }
  }

}