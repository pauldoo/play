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
import play.api.Play

object BlipMosaic extends Controller {
  val featureWidth = 3;
  val featureHeight = 3;
  val tileSize = 12;
  val ditherStrength = 0.2;

  private def apiKey(): String = {
    Play.current.configuration.getString("blipfoto.apikey").get;
  }

  def index = Action {
    Ok(views.html.BlipMosaic.index());
  }

  def start(username: String) = Action {
    Async {
      val baseUrl: String = "http://api.blipfoto.com/get/entry/";

      val latestEntryPromise: Promise[Response] = WS.url(baseUrl).withQueryString(
        ("api_key", apiKey),
        ("version", "2"),
        ("format", "XML"),
        ("display_name", username),
        ("date", "latest"),
        ("data", "entry_id")).get();

      latestEntryPromise.map { response =>
        val body: Node = response.xml;
        val latestEntryId = (body \\ "entry_id").text;
        Logger.info("Latest entry for " + username + " is " + latestEntryId);

        Redirect(routes.BlipMosaic.generate("by " + username, latestEntryId));
      }
    }
  }

  class ThumbnailWithFeatures(
    val entryId: String,
    val thumbnailUrl: String,
    val features: Seq[Double]) {

  }

  def generate(searchQuery: String, targetEntryId: String) = Action {
    Async {
      val baseUrl: String = "http://api.blipfoto.com/get/search/";
      val searchPromise: Promise[Response] = WS.url(baseUrl).withQueryString(
        ("api_key", apiKey),
        ("version", "2"),
        ("format", "XML"),
        ("query", searchQuery),
        ("size", "small"),
        ("max", "1000")).get();

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
      val targetImage: Promise[(BufferedImage, (String, String, String))] = WS.url(entryDetailsUrl).withQueryString(
        ("api_key", apiKey),
        ("version", "2"),
        ("format", "XML"),
        ("entry_id", targetEntryId),
        ("data", "image,prev_entry_id,next_entry_id,permalink")).get().flatMap { response =>
          val body: Node = response.xml;
          val imageUrl = (body \\ "image").text;
          // TODO: Consider using large images?
          //val largeUrl = (body \\ "large_image").text;
          Logger.info("Target image #" + targetEntryId + " found: " + imageUrl);

          val prevEntryId = (body \\ "prev_entry_id").text;
          val nextEntryId = (body \\ "next_entry_id").text;
          val permalink = (body \\ "permalink").text;
          val meta = (permalink, prevEntryId, nextEntryId);
          getImage(imageUrl).map { image => (image, meta) }
        }

      val result: Promise[(Seq[ThumbnailWithFeatures], BufferedImage, (String, String, String))] = for {
        thumbs <- thumbFeatures;
        target <- targetImage
      } yield (thumbs, target._1, target._2);

      result.map { tuple =>
        val chosenThumbnails: Seq[Seq[String]] = chooseThumbnails(tuple._1, tuple._2);
        Ok(views.html.BlipMosaic.tableMosaic(tuple._3._1, chosenThumbnails, tileSize, tuple._3._2, tuple._3._3))
      }
    }
  }

  def chooseThumbnails(thumbnails: Seq[ThumbnailWithFeatures], target: BufferedImage): Seq[Seq[String]] = {
    val columns: Int = target.getWidth() / tileSize;
    val rows: Int = target.getHeight() / tileSize;

    Logger.info("Found " + thumbnails.size + " thumbnails for fitting.");

    val scaledTarget = toScaledBufferedImage(target, columns * featureWidth, rows * featureHeight);

    val targetFeatures: IndexedSeq[IndexedSeq[Seq[Double]]] =
      for (r <- 0 until rows)
        yield for (c <- 0 until columns) yield {
        extractFeatures(scaledTarget.getSubimage(c * featureWidth, r * featureHeight, featureWidth, featureHeight));
      };

    val swizzled = mortonOrder(targetFeatures, columns, rows);
    val zeroFeature = (0 until targetFeatures.head.head.length).map { x => 0.0 };
    val mapped = fitTiles(swizzled, zeroFeature, thumbnails).map(_.thumbnailUrl);
    val unswizzled = unMortonOrder(mapped, columns, rows);
    Logger.info("Finished fitting.");
    unswizzled
  }

  def fitTiles(targetFeatures: Seq[Seq[Double]], compensation: Seq[Double], thumbnails: Seq[ThumbnailWithFeatures]): List[ThumbnailWithFeatures] = {

    if (targetFeatures.isEmpty) {
      List.empty
    } else {
      val compensatedFeatures: Seq[Double] = (targetFeatures.head zip compensation).map { t => t._1 + t._2 };
      val chosenThumbnail = closestThumbnailTo(compensatedFeatures, thumbnails);
      val newCompensation = (compensatedFeatures zip chosenThumbnail.features).map { t => t._1 - t._2 }.map { _ * ditherStrength };

      chosenThumbnail :: fitTiles(targetFeatures.tail, newCompensation, thumbnails);
    }
  }

  def rgbToYuv(r: Double, g: Double, b: Double): List[Double] = {
    val R = r * 255.0;
    val G = g * 255.0;
    val B = b * 255.0;
    val Y = 0.299 * R + 0.587 * G + 0.114 * B;
    val Cb = -0.1687 * R - 0.3313 * G + 0.5 * B + 128;
    val Cr = 0.5 * R - 0.4187 * G - 0.0813 * B + 128;
    List(Y / 255.0, 0.5 * Cb / 255.0, 0.5 * Cr / 255.0)
  }

  def mortonOrder[A](elements: IndexedSeq[IndexedSeq[A]], width: Int, height: Int): Seq[A] = {
    val coordinates = for (x <- 0 until width; y <- 0 until height) yield (y, x);
    coordinates.sortBy { c => swizzle(c._2, c._1) }.map { c => elements(c._1)(c._2) };
  }

  def unMortonOrder[A](elements: Seq[A], width: Int, height: Int): Seq[Seq[A]] = {
    val coordinates = for (x <- 0 until width; y <- 0 until height) yield (y, x);
    val k = coordinates.sortBy { c => swizzle(c._2, c._1) } zip elements;
    k.sortBy { e => e._1 }.map { _._2 }.grouped(width).toSeq
  }

  def closestThumbnailTo(
    targetFragment: Seq[Double],
    thumbnails: Seq[ThumbnailWithFeatures]): ThumbnailWithFeatures = {
    thumbnails.minBy { t => rmsDifference(t.features, targetFragment) };
  }

  def rmsDifference(a: Seq[Double], b: Seq[Double]): Double = {
    assert(a.length == b.length);
    val square: Double => Double = { k => k * k };
    math.sqrt((a zip b).map { t => square(t._1 - t._2) }.reduce(_ + _) / a.length);
  }

  def getThumbnailFeatures(thumbnailUrl: String): Promise[Seq[Double]] = {
    val key = thumbnailUrl + ".features";
    Cache.getOrElse[Promise[Seq[Double]]](key) {
      //Logger.info("Calculating features for image: " + thumbnailUrl);
      val imagePromise: Promise[BufferedImage] = getImage(thumbnailUrl);
      imagePromise.map(image => {
        val scaledImage: BufferedImage = toScaledBufferedImage(image, featureWidth, featureHeight);

        val featureValues: Seq[Double] = extractFeatures(scaledImage);
        //Logger.info("Computed feature values: " + featureValues + ", for image: " + thumbnailUrl);
        featureValues;
      });
    }
  }

  def extractFeatures(scaledImage: BufferedImage): Seq[Double] = {
    assert(scaledImage.getWidth() == featureWidth);
    assert(scaledImage.getHeight() == featureHeight);
    val t: Seq[Int] = for (
      x <- 0 until featureWidth;
      y <- 0 until featureHeight
    ) yield scaledImage.getRGB(x, y);

    t.flatMap { pixel =>
      rgbToYuv(((pixel >> 16) & 0xFF) / 255.0, ((pixel >> 8) & 0xFF) / 255.0, ((pixel >> 0) & 0xFF) / 255.0)
    }
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

  def dilate2(n: Int): Int = {
    var x = n;
    assert(0 <= n && n < 0x100);
    x = (x | (x << 8)) & 0x00FF00FF;
    x = (x | (x << 4)) & 0x0F0F0F0F;
    x = (x | (x << 2)) & 0x33333333;
    x = (x | (x << 1)) & 0x55555555;
    return x;
  }

  def swizzle(x: Int, y: Int): Int = {
    dilate2(x) | (dilate2(y) << 1);
  }
}