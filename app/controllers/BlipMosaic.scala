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
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import javax.imageio.plugins.jpeg.JPEGImageWriteParam
import javax.imageio.ImageWriteParam
import javax.imageio.IIOImage
import javax.imageio.stream.MemoryCacheImageOutputStream
import models.BlipMosaicForm
import play.api.data.Forms._
import play.api.data._
import models.BlipMosaicForm
import models.BlipMosaicForm

object BlipMosaic extends Controller {
  private val featureWidth = 3;
  private val featureHeight = 3;

  private def apiKey(): String = {
    Play.current.configuration.getString("blipfoto.apikey").get;
  }

  def index = Action {
    Ok(views.html.BlipMosaic.index(startForm.fill(new BlipMosaicForm("", 768, 12, 2))));
  }

  val startForm: Form[BlipMosaicForm] = Form(
    mapping(
      "username" -> nonEmptyText,
      "imageSize" -> number(min = 256, max = 2048),
      "tileSize" -> number(min = 8, max = 64),
      "ditherStrength" -> number(min = 0, max = 10) //
      ) {
        (username, imageSize, tileSize, ditherStrength) => BlipMosaicForm(username, imageSize, tileSize, ditherStrength)
      } {
        (form: BlipMosaicForm) => Some(form.username, form.imageSize, form.tileSize, form.ditherStrength)
      });

  def start = Action { implicit request =>
    startForm.bindFromRequest().fold(
      errors => BadRequest("Boo!"), //BadRequest(html.contact.form(errors)),
      form => Async {
        val baseUrl: String = "http://api.blipfoto.com/get/entry/";

        val latestEntryPromise: Promise[Response] = WS.url(baseUrl).withQueryString(
          ("api_key", apiKey),
          ("version", "2"),
          ("format", "XML"),
          ("display_name", form.username),
          ("date", "latest"),
          ("data", "entry_id")).get();

        latestEntryPromise.map { response =>
          val body: Node = response.xml;
          val latestEntryId = (body \\ "entry_id").text;
          Logger.info("Latest entry for " + form.username + " is " + latestEntryId);

          Redirect(routes.BlipMosaic.generateHtml(
            form.imageSize,
            form.tileSize,
            form.ditherStrength,
            "by " + form.username,
            latestEntryId));
        }
      })
  }

  private class ThumbnailWithFeatures(
    val entryId: String,
    val thumbnailUrl: String,
    val features: IndexedSeq[Double]) extends KDTree.HasVector {

    val vector = features;
  }

  private class TargetEntryMetadata(
    val url: String,
    val permalink: String,
    val imageWidth: Int,
    val imageHeight: Int,
    val prevEntryId: String,
    val nextEntryId: String) {};

  def generateHtml(imageSize: Int, tileSize: Int, ditherStrength: Int, searchQuery: String, targetEntryId: String) = Action {
    Async {
      val tableAndEntryMetadata = getTargetImageMetadata(targetEntryId);

      for {
        metadata <- tableAndEntryMetadata
      } yield {
        val (newWidth, newHeight) = resize(metadata.imageWidth, metadata.imageHeight, imageSize);

        Ok(views.html.BlipMosaic.tableMosaic(
          metadata.permalink,
          routes.BlipMosaic.generateImage(imageSize, tileSize, ditherStrength, searchQuery, targetEntryId),
          newWidth,
          newHeight,
          metadata.prevEntryId,
          metadata.nextEntryId))
      }
    }
  }

  def generateImage(imageSize: Int, tileSize: Int, ditherStrength: Int, searchQuery: String, targetEntryId: String) = Action {
    Async {
      val key = "generateImage/" + imageSize + "/" + tileSize + "/" + ditherStrength + "/" + searchQuery + "/" + targetEntryId;
      val imageBytes: Promise[Array[Byte]] = Cache.getOrElse[Promise[Array[Byte]]](key) {
        val thumbsP: Promise[Seq[ThumbnailWithFeatures]] = searchForThumbnails(searchQuery);
        val imageP: Promise[BufferedImage] =
          getTargetImageMetadata(targetEntryId) flatMap { meta =>
            getImage(meta.url) map { image =>
              {
                assert(meta.imageWidth == image.getWidth());
                assert(meta.imageHeight == image.getHeight());
                val (newWidth, newHeight) = resize(meta.imageWidth, meta.imageHeight, imageSize);
                toScaledBufferedImage(image, newWidth, newHeight);
              }
            }
          };

        for {
          thumbs <- thumbsP;
          image <- imageP
        } yield {
          renderImage(chooseThumbnails(thumbs, image, tileSize, ditherStrength), tileSize).value.get
        }
      };

      imageBytes.map(bytes => Ok(bytes).as("image/jpeg"));
    }
  }

  private def resize(width: Int, height: Int, target: Int) = {
    val max = math.max(width, height);
    ((width * target) / max, (height * target) / max)
  }

  def renderImage(thumbs: Seq[Seq[String]], tileSize: Int): Promise[Array[Byte]] = {
    val width = thumbs.head.length * tileSize;
    val height = thumbs.length * tileSize;

    Promise.sequence(
      for {
        row <- thumbs;
        thumb <- row
      } yield scaledBufferedImageOfThumbnail(thumb, tileSize)) map {
        images =>
          {
            val buffer: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
            val g = buffer.getGraphics();

            for {
              (img, idx) <- images.zipWithIndex
            } {
              val x = idx % thumbs.head.length;
              val y = idx / thumbs.head.length;
              g.drawImage(img, x * tileSize, y * tileSize, null);
            }
            g.dispose();

            val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream();
            val writer = ImageIO.getImageWritersByFormatName("jpg").next();
            val writeParams = writer.getDefaultWriteParam().asInstanceOf[JPEGImageWriteParam];
            writeParams.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
            writeParams.setCompressionQuality(0.9f);
            writeParams.setOptimizeHuffmanTables(true);
            writeParams.setProgressiveMode(ImageWriteParam.MODE_DEFAULT);
            writer.setOutput(new MemoryCacheImageOutputStream(byteStream));
            writer.write(null, new IIOImage(buffer, null, null), writeParams);

            byteStream.toByteArray()
          }
      };

  }

  private def getTargetImageMetadata(targetEntryId: String): Promise[TargetEntryMetadata] = {
    val key = "getTargetImageMetadata/" + targetEntryId;
    Cache.getOrElse[Promise[TargetEntryMetadata]](key) {

      val entryDetailsUrl: String = "http://api.blipfoto.com/get/entry/";
      WS.url(entryDetailsUrl).withQueryString(
        ("api_key", apiKey),
        ("version", "2"),
        ("format", "XML"),
        ("entry_id", targetEntryId),
        ("data", "image,image_width,image_height,prev_entry_id,next_entry_id,permalink")).get().map { response =>
          {
            val body: Node = response.xml;
            val imageUrl = (body \\ "image").text;
            Logger.info("Target image #" + targetEntryId + " found: " + imageUrl);

            val imageWidth = (body \\ "image_width").text.toInt;
            val imageHeight = (body \\ "image_height").text.toInt;
            val prevEntryId = (body \\ "prev_entry_id").text;
            val nextEntryId = (body \\ "next_entry_id").text;
            println(prevEntryId);
            println(nextEntryId);
            val permalink = (body \\ "permalink").text;
            new TargetEntryMetadata(imageUrl, permalink, imageWidth, imageHeight, prevEntryId, nextEntryId)
          }
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

  private def chooseThumbnails(thumbnails: Seq[ThumbnailWithFeatures], target: BufferedImage, tileSize: Int, ditherStrength: Int): Seq[Seq[String]] = {
    val columns: Int = target.getWidth() / tileSize;
    val rows: Int = target.getHeight() / tileSize;

    val ditherFactor: Double = (ditherStrength / 10.0);
    def fitTiles(
      targetFeatures: Seq[IndexedSeq[Double]],
      compensation: Seq[Double],
      thumbnailTree: KDTree[ThumbnailWithFeatures]): List[ThumbnailWithFeatures] = {

      if (targetFeatures.isEmpty) {
        List.empty
      } else {
        val compensatedFeatures: IndexedSeq[Double] = (targetFeatures.head zip compensation).map { t => t._1 + t._2 };
        val chosenThumbnail = closestThumbnailTo(compensatedFeatures, thumbnailTree);
        val newCompensation = (compensatedFeatures zip chosenThumbnail.features).map { t => t._1 - t._2 }.map { _ * ditherFactor };

        chosenThumbnail :: fitTiles(targetFeatures.tail, newCompensation, thumbnailTree);
      }
    }

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
      getImage(thumbnailUrl) map { image =>
        extractFeatures(toScaledBufferedImage(image, featureWidth, featureHeight));
      };
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

  private def scaledBufferedImageOfThumbnail(thumbnailUrl: String, tileSize: Int): Promise[BufferedImage] = {
    val key = "scaledBufferedThumb/" + tileSize + "/" + thumbnailUrl;
    return Cache.getOrElse[Promise[BufferedImage]](key) {
      getImage(thumbnailUrl) map { image => toScaledBufferedImage(image, tileSize, tileSize) };
    }
  }

  private def toScaledBufferedImage(source: Image, width: Int, height: Int): BufferedImage = {
    val scaled = source.getScaledInstance(width, height, Image.SCALE_SMOOTH);
    val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
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