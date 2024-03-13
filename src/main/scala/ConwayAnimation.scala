import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.core.format.Gif
import doodle.interact.syntax.all.*
import doodle.java2d.effect.GifEncoder
import doodle.java2d.{Picture, *}
import doodle.syntax.all.*
import fs2.Stream

import scala.concurrent.duration.*
import java.util.concurrent.TimeUnit
import scala.util.Random

object ConwayAnimation {

  val screenHeight = 1200 //Size of Window
  val screenWidth = 1200 //Size of Window
  val resolution = 30 //Size of Cell
  val filledPercentage: Int = 70 //How much of Black vs White in random Grid (0-100)
  val frameTime: Long = 50 //Time between simulation/animation frames
  val fileFrameCount: Int = 20 //How many frames of animation to write to gif

  val cellCountX: Int = screenHeight / resolution //Get Cell Count on X axis
  val cellCountY: Int = screenWidth / resolution //Get Cell Count on Y axis

  //Cell Picture
  val cell: Picture[Unit] =
    Picture
      .square(resolution)
      .strokeWidth(3)
      .strokeColor(Color.grey)

  //Fill Starting Grid Randomly
  var currentGrid: Array[Array[Int]] = randomGrid()

  //Create Frame(Canvas/Window) to write to
  val frame: Frame  = Frame.default
    .withTitle("Conways Game of Life Doodle")
    .withSize(screenWidth, screenHeight)
    .withBackground(Color.white)

  //Generated Grid filled randomly with percentage $filledPercentage
  def randomGrid(): Array[Array[Int]] ={
    //Create Grid Array
    val newGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    //Fill with random Black-1 and White-0 by percentage Black/White
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      newGrid(i)(j) = if(Random.between(0, 100) > filledPercentage) 1 else 0
    }
    newGrid
  }

  //Calculates number of neighbors of a cell
  //Using wraparound
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

  //Calculate new grid based on $currentGrid according to rules at
  //https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
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

  //Update,draw and output a picture of the state of the grid
  def grid(): Picture[Unit] = {
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
  }

  val animation: Stream[IO, Picture[Unit]] = {
    Stream(1).repeat
      .debounce[IO](Duration(frameTime, TimeUnit.MILLISECONDS))
      .map { s => grid() }
  }

  //Animation output to frame/window
  def go(): Unit =
    animation.animateFrames(frame)

  //Animation output to file
  def write(): Unit =
    animation.take(fileFrameCount).write[Gif]("conway.gif", frame)




}
