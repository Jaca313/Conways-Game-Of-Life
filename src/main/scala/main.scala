import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.core.format.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import doodle.syntax.all.*
import doodle.effect.*
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.util.Random
import concurrent.duration.DurationInt

val screenHeight = 800
val screenWidth = 800
val resolution = 40

val cellCountX: Int = screenHeight / resolution
val cellCountY: Int = screenWidth / resolution

val cell =
  Picture
    .square(40)
    .strokeColor(Color.black)
    .strokeWidth(1.0)


//def explorer(using
//             intGui: ExploreInt[Component],
//             colorGui: ExploreColor[Component],
//             layoutGui: Layout[Component],
//            ) = {
//  import intGui._
//  import colorGui._
//
//  int("Base Size").within(1 to 30)
//    .beside(int("Iterations").within(1 to 6).withDefault(2))
//    .above(color("Stroke Color"))
//}

@main
def main(): Unit = {
  //Create Frame
  var frame = Frame.default.withTitle("Conways Game of Life Doodle").withSize(screenHeight, screenWidth).withBackground(Color.whiteSmoke).withClearToColor(Color.whiteSmoke) //.withCenterAtOrigin
  val canvas = frame.canvas()

  val origin = Picture.circle(1).at(screenWidth, screenHeight)

  val grid1: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
  val grid2: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)

  //Fill grid1 with Random Noise 0-1
  for (i <- 0 until 20; j <- 0 until 20) {
    grid1(i)(j) = Random.between(0, 2)
  }

  //Fill grid1 with Random Noise 0-1
  for (i <- 0 until 20; j <- 0 until 20) {
    grid2(i)(j) = Random.between(0, 2)
  }



  //Draw Grid1
  var vertices = origin
  for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
    vertices = cell
      .fillColor(if (grid1(i)(j) == 1) Color.black else Color.white)
      .strokeWidth(3).strokeColor(Color.grey)
      .at((i + 0.5) * resolution, (j + 0.5) * resolution)
      .on(vertices)
  }
  //vertices.drawWithFrame(frame)

  //Draw Grid2
  var vertices2 = origin
  for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
    vertices2 = cell
      .fillColor(if (grid1(i)(j) == 1) Color.black else Color.white)
      .strokeWidth(3).strokeColor(Color.grey)
      .at((i + 0.5) * resolution, (j + 0.5) * resolution)
      .on(vertices2)
  }
    ConwayAnimation.write()
    ConwayAnimation.go()

}

