import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.instances.all.*
import doodle.core.*
import doodle.core.format.Gif
import doodle.effect.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.interact.syntax.all.*
import doodle.java2d.effect.*
import doodle.java2d.{Picture, *}
import doodle.syntax.all.*
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.util.Random


object ConwayAnimation {

  import scala.concurrent.duration.*

  val screenHeight = 1200
  val screenWidth = 1200
  val resolution = 30

  val cellCountX: Int = screenHeight / resolution
  val cellCountY: Int = screenWidth / resolution

  val filledPercentage: Int = 70 //How much of Black vs White in random Grid (0-100)
  val cell =
    Picture
      .square(resolution)
      .strokeWidth(3)
      .strokeColor(Color.grey)

  val frame =
    Frame.default.withTitle("Conways Game of Life Doodle").withSize(screenWidth, screenHeight).withBackground(Color.white)

  var currentGrid: Array[Array[Int]] = randomizeGrid()

  def randomizeGrid(): Array[Array[Int]] =
    //Create Array
    val grid1: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    //Fill with random Black and White
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      grid1(i)(j) = if(Random.between(0, 100) > filledPercentage) 1 else 0
    }
    grid1

  def calculateNeighbor(x:Int,y:Int): Int = {
    var sum = 0
    for (i <- -1 until 2; j <- -1 until 2) {
      val row = (x + i + cellCountX)  % cellCountX
      val col = (y + j + cellCountY) % cellCountY
      sum += currentGrid(row)(col)
    }
    sum -= currentGrid(x)(y)
    sum
  }

  def calculateNewGrid(): Array[Array[Int]] ={
    val newGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      newGrid(i)(j) =
        val neighborCount = calculateNeighbor(i,j)
        if(currentGrid(i)(j) == 1 && (neighborCount < 2 ||  neighborCount > 3)) 0
        else if(currentGrid(i)(j) == 0 && neighborCount == 3 || currentGrid(i)(j) == 1 && (neighborCount == 2 || neighborCount == 3)) 1
        else 0
    }
    newGrid
  }

  def grid(): Picture[Unit] =
    currentGrid = calculateNewGrid()

    val origin = Picture.circle(1).at(screenWidth, screenHeight)
    var vertices = origin
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      vertices = cell
        .fillColor(if (currentGrid(i)(j) == 1) Color.black else Color.white)
        .at((i + 0.5) * resolution, (j + 0.5) * resolution)
        .on(vertices)
    }
    vertices


  val animation: Stream[IO, Picture[Unit]] =
    Stream(1).repeat
      .debounce[IO](50.millis)
      .scan((1, 0)) { (state, _) =>
        val (inc, size) = state
        (1,1)
      }
      .map { case (_, s) => grid() }

  def go() =
    animation.animateFrames(frame)

  def write() =
    animation.take(20).write[Gif]("conway.gif", frame)




}
