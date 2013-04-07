package models

case class MosaicSettings(imageSize: Int, tileSize: Int, ditherStrength: Int);

object MosaicSettings {
  val imageSizes = List(384, 768, 1536);
  val tileSizes = List(6, 12, 24);
  val ditherStrengths = List(0, 2, 4, 6, 8, 10);

  val defaults = MosaicSettings(768, 12, 2);
}