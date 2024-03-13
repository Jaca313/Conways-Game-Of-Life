import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import doodle.syntax.all.*


val screenHeight = 800
val screenWidth = 800
val resolution = 40

val cell =
  Picture
    .square(40)
    .strokeColor(Color.black)
    .strokeWidth(1.0)


@main
def main(): Unit = {
  val frame = Frame.default.withSize(screenHeight, screenWidth).withBackground(Color.whiteSmoke)
  cell.drawWithFrame(frame)

}