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
import utils.KDTree
import play.api.mvc.SimpleResult
import play.api.templates.Html

object BlipMosaic extends Controller {
  private val featureWidth = 3;
  private val featureHeight = 3;
  private val tileSize = 12;
  private val ditherStrength = 0.2;

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

  private class ThumbnailWithFeatures(
    val entryId: String,
    val thumbnailUrl: String,
    val features: IndexedSeq[Double]) extends KDTree.HasVector {

    val vector = features;
  }

  private class TargetEntryMetadata(
    val permalink: String,
    val prevEntryId: String,
    val nextEntryId: String) {};

  def generate(searchQuery: String, targetEntryId: String) = Action {
    Async {
      val key = "generate/" + searchQuery + "/" + targetEntryId;
      val tableAndEntryMetadata = Cache.getOrElse[Promise[Tuple2[Seq[Seq[String]], TargetEntryMetadata]]](key) {
        val thumbFeatures: Promise[Seq[ThumbnailWithFeatures]] = searchForThumbnails(searchQuery);
        val targetImage: Promise[Tuple2[TargetEntryMetadata, BufferedImage]] = getTargetImage(targetEntryId);

        for {
          thumbs <- thumbFeatures;
          (metadata, image) <- targetImage
        } yield (chooseThumbnails(thumbs, image), metadata)
      }

      for {
        (chosenThumbnails, metadata) <- tableAndEntryMetadata
      } yield Ok(views.html.BlipMosaic.tableMosaic(
        metadata.permalink,
        chosenThumbnails,
        tileSize,
        metadata.prevEntryId,
        metadata.nextEntryId))
    }
  }

  private def getTargetImage(targetEntryId: String): Promise[Tuple2[TargetEntryMetadata, BufferedImage]] = {
    val entryDetailsUrl: String = "http://api.blipfoto.com/get/entry/";
    WS.url(entryDetailsUrl).withQueryString(
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
        println(prevEntryId);
        println(nextEntryId);
        val permalink = (body \\ "permalink").text;
        getImage(imageUrl).map { image =>
          (new TargetEntryMetadata(permalink, prevEntryId, nextEntryId), image)
        }
      }
  }

  private def searchForThumbnails(searchQuery: String): Promise[Seq[ThumbnailWithFeatures]] = {
    val key = "searchQuery/" + searchQuery;
    Cache.getOrElse[Promise[Seq[ThumbnailWithFeatures]]](key) {
      val baseUrl: String = "http://api.blipfoto.com/get/search/";

      val searchPromise: Promise[Node] = WS.url(baseUrl).withQueryString(
        ("api_key", apiKey),
        ("version", "2"),
        ("format", "XML"),
        ("query", searchQuery),
        ("size", "small"),
        ("max", "1000")).get() map { response => response.xml };

      searchPromise.flatMap { searchResponseBody =>
        val body: Node = searchResponseBody;
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
    }
  }

  private def chooseThumbnails(thumbnails: Seq[ThumbnailWithFeatures], target: BufferedImage): Seq[Seq[String]] = {
    val columns: Int = target.getWidth() / tileSize;
    val rows: Int = target.getHeight() / tileSize;

    Logger.info("Found " + thumbnails.size + " thumbnails for fitting.");

    val scaledTarget = toScaledBufferedImage(target, columns * featureWidth, rows * featureHeight);

    val targetFeatures: IndexedSeq[IndexedSeq[IndexedSeq[Double]]] =
      for (r <- 0 until rows)
        yield for (c <- 0 until columns) yield {
        extractFeatures(scaledTarget.getSubimage(c * featureWidth, r * featureHeight, featureWidth, featureHeight));
      };

    Logger.info("Creating KDTree.");
    val thumbnailTree: KDTree[ThumbnailWithFeatures] = KDTree.create(thumbnails);
    Logger.info("KDTree constructed.");

    val swizzled = mortonOrder(targetFeatures, columns, rows);
    Logger.info("Morton order constructed.");
    val zeroFeature = (0 until targetFeatures.head.head.length).map { x => 0.0 };
    val mapped = fitTiles(swizzled, zeroFeature, thumbnailTree).map(_.thumbnailUrl);
    Logger.info("Fitted tiles.");
    val unswizzled = unMortonOrder(mapped, columns, rows);
    Logger.info("Finished fitting.");
    unswizzled
  }

  private def fitTiles(
    targetFeatures: Seq[IndexedSeq[Double]],
    compensation: Seq[Double],
    thumbnailTree: KDTree[ThumbnailWithFeatures]): List[ThumbnailWithFeatures] = {

    if (targetFeatures.isEmpty) {
      List.empty
    } else {
      val compensatedFeatures: IndexedSeq[Double] = (targetFeatures.head zip compensation).map { t => t._1 + t._2 };
      val chosenThumbnail = closestThumbnailTo(compensatedFeatures, thumbnailTree);
      val newCompensation = (compensatedFeatures zip chosenThumbnail.features).map { t => t._1 - t._2 }.map { _ * ditherStrength };

      chosenThumbnail :: fitTiles(targetFeatures.tail, newCompensation, thumbnailTree);
    }
  }

  private def rgbToYuv(r: Double, g: Double, b: Double): List[Double] = {
    val R = r * 255.0;
    val G = g * 255.0;
    val B = b * 255.0;
    val Y = (0.299 * R) + (0.587 * G) + (0.114 * B);
    val Cb = (-0.1687 * R) + (-0.3313 * G) + (0.5 * B) + 128;
    val Cr = (0.5 * R) + (-0.4187 * G) + (-0.0813 * B) + 128;
    List(Y / 255.0, 0.5 * Cb / 255.0, 0.5 * Cr / 255.0);
    //List(r, g, b);
  }

  private def mortonOrder[A](elements: IndexedSeq[IndexedSeq[A]], width: Int, height: Int): Seq[A] = {
    val coordinates = for (x <- 0 until width; y <- 0 until height) yield (y, x);
    coordinates.sortBy { c => swizzle(c._2, c._1) }.map { c => elements(c._1)(c._2) };
  }

  private def unMortonOrder[A](elements: Seq[A], width: Int, height: Int): Seq[Seq[A]] = {
    val coordinates = for (x <- 0 until width; y <- 0 until height) yield (y, x);
    val k = coordinates.sortBy { c => swizzle(c._2, c._1) } zip elements;
    k.sortBy { e => e._1 }.map { _._2 }.grouped(width).toSeq
  }

  private def closestThumbnailTo(
    targetFragment: IndexedSeq[Double],
    thumbnailTree: KDTree[ThumbnailWithFeatures]): ThumbnailWithFeatures = {
    KDTree.closestPoint(thumbnailTree, targetFragment);
  }

  private def getThumbnailFeatures(thumbnailUrl: String): Promise[IndexedSeq[Double]] = {
    val key = "features/" + thumbnailUrl;
    Cache.getOrElse[Promise[IndexedSeq[Double]]](key) {
      //Logger.info("Calculating features for image: " + thumbnailUrl);
      val imagePromise: Promise[BufferedImage] = getImage(thumbnailUrl);
      imagePromise.map(image => {
        val scaledImage: BufferedImage = toScaledBufferedImage(image, featureWidth, featureHeight);

        val featureValues: IndexedSeq[Double] = extractFeatures(scaledImage);
        //Logger.info("Computed feature values: " + featureValues + ", for image: " + thumbnailUrl);
        featureValues;
      });
    }
  }

  private def extractFeatures(scaledImage: BufferedImage): IndexedSeq[Double] = {
    assert(scaledImage.getWidth() == featureWidth);
    assert(scaledImage.getHeight() == featureHeight);
    val t: Seq[Int] = for (
      x <- 0 until featureWidth;
      y <- 0 until featureHeight
    ) yield scaledImage.getRGB(x, y);

    t.flatMap { pixel: Int =>
      rgbToYuv(((pixel >> 16) & 0xFF) / 255.0, ((pixel >> 8) & 0xFF) / 255.0, ((pixel >> 0) & 0xFF) / 255.0)
    }.toIndexedSeq
  }

  private def toScaledBufferedImage(source: Image, width: Int, height: Int): BufferedImage = {
    val scaled = source.getScaledInstance(width, height, Image.SCALE_SMOOTH);
    val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    val g = result.getGraphics();
    g.drawImage(scaled, 0, 0, null);
    g.dispose();
    return result;
  }

  private def getImage(imageUrl: String): Promise[BufferedImage] = {
    getImageBytes(imageUrl) map { bytes =>
      ImageIO.read(new ByteArrayInputStream(bytes))
    }
  }

  private def getImageBytes(imageUrl: String): Promise[Array[Byte]] = {
    val key = "imageBytes/" + imageUrl;
    Cache.getOrElse[Promise[Array[Byte]]](key) {
      Logger.info("Downloading image: " + imageUrl);
      WS.url(imageUrl).get().map { resp =>
        resp.ahcResponse.getResponseBodyAsBytes()
      }
    }
  }

  private def dilate2(n: Int): Int = {
    var x = n;
    assert(0 <= n && n < 0x100);
    x = (x | (x << 8)) & 0x00FF00FF;
    x = (x | (x << 4)) & 0x0F0F0F0F;
    x = (x | (x << 2)) & 0x33333333;
    x = (x | (x << 1)) & 0x55555555;
    return x;
  }

  private def swizzle(x: Int, y: Int): Int = {
    dilate2(x) | (dilate2(y) << 1);
  }
}