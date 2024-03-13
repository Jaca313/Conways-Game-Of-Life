import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import doodle.syntax.all.*

import scala.util.Random


val screenHeight = 800
val screenWidth = 800
val resolution = 40

val cellCountX = screenHeight / resolution
val cellCountY = screenWidth / resolution

val cell =
  Picture
    .square(40)
    .strokeColor(Color.black)
    .strokeWidth(1.0)


@main
def main(): Unit = {
  //Create Frame
  val frame = Frame.default.withSize(screenHeight, screenWidth).withBackground(Color.whiteSmoke)
  val canvas = frame.canvas()

  val origin = Picture.circle(1).at(screenWidth, screenHeight)

  val grid1: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
  val grid2: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)

  //Fill grid1 with Random Noise 0-1
  for (i <- 0 until 20; j <- 0 until 20) {
    grid1(i)(j) = Random.between(0, 2)
  }

  var vertices = origin
  //Draw Grid1
  for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
    vertices = cell
      .fillColor(if (grid1(i)(j) == 1) Color.black else Color.white)
      .at((i + 0.5) * resolution, (j + 0.5) * resolution)
      .on(vertices)
  }

  vertices.drawWithFrame(frame)

}