package models

case class BlipMosaicForm(
  username: String,
  imageSize: Int,
  tileSize: Int,
  ditherStrength: Int)