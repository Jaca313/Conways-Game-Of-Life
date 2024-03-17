import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.core.format.Gif
import doodle.interact.syntax.all.*
import doodle.java2d.effect.{Center, GifEncoder, Redraw, Size}
import doodle.java2d.{Picture, *}
import doodle.syntax.all.*
import fs2.Stream

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Random
import java.util.concurrent.TimeUnit

enum BorderRules:
  case FIXEDEDGEDEAD
  case WRAPAROUND

enum InputMode:
  case LOADFROMFILE
  case RANDOM

object ConwayAnimation {
  val default: ConwayAnimation =
    ConwayAnimation(
      screenHeight = 800,
      screenWidth = 800,
      cellCountX = 10,
      cellCountY = 10,
      resolution = 80,
      filledPercentage = 70,
      frameTime = 250,
      fileFrameCount = 20,
      fileName = "conway.txt",
      inputMode = InputMode.RANDOM,
      borderRules = BorderRules.WRAPAROUND,
      currentGrid = Array.fill(10,10)(0)
    )
}

final case class ConwayAnimation(
    private val screenWidth: Int,                 /**Size of Window*/
    private val screenHeight: Int,                /**Size of Window*/
    private var cellCountX: Int,                  /**Cell Count on X axis*/
    private var cellCountY: Int,                  /**Cell Count on Y axis*/
    private var resolution: Int,                  /**Size of Cell*/
    private val filledPercentage: Int,            /**How much of Black vs White in random Grid (0-100)*/
    private val frameTime: Long,                  /**Time between simulation/animation frames*/
    private val fileFrameCount: Int,              /**How many frames of animation to write to gif*/
    private val fileName: String,                 /**What file to load from*/
    private val inputMode: InputMode,             /**Enum where to source initial board*/
    private val borderRules: BorderRules,         /**Enum how to handle edge of board*/
    private var currentGrid: Array[Array[Int]]
) {

  /**Size the window*/
  def withScreenSize(screenWidth: Int,screenHeight: Int): ConwayAnimation =
    val cp = this.copy(screenWidth = screenWidth,screenHeight = screenHeight)
    cp.copy(resolution = cp.calculateResolution())

  /**Use an X by Y grid of cells when not loading from a file*/
  def withCellDimensions(cellCountX: Int,cellCountY: Int): ConwayAnimation =
    val cp = this.copy(cellCountX = cellCountX,cellCountY = cellCountY)
    val cp2 = cp.copy(resolution = cp.calculateResolution())
    cp2.copy(currentGrid = Array.fill(cellCountX,cellCountY)(0))

  /**Fill with this percentage when filling grid randomly*/
  def withFilledCellsPercentage(filledPercentage : Int): ConwayAnimation =
    this.copy(filledPercentage = filledPercentage)

  /**Use this time between animation frames*/
  def withAnimationFrameTime(frameTime: Long): ConwayAnimation =
    this.copy(frameTime = frameTime)

  /**Write this many frames to gif when output to file*/
  def withWriteToGifFrameCount(fileFrameCount: Int): ConwayAnimation =
    this.copy(fileFrameCount = fileFrameCount)

  /**Source input grid as Random*/
  def withInputRandom(): ConwayAnimation =
    val cp = this.copy(inputMode = InputMode.RANDOM)
    cp.copy(currentGrid = cp.randomGrid())

  /**Source input grid from file*/
  def withInputFromFile(fileName: String): ConwayAnimation =
    val cp = this.copy(fileName = fileName)
    cp.copy(currentGrid = cp.loadGrid(cp.fileName))

  /**Change how to handle edges of the board*/
  def withBorderRule(borderRules: BorderRules): ConwayAnimation =
    this.copy(borderRules = borderRules)

  /**Cell Picture*/
  private def cell(size: Int): Picture[Unit] =
    Picture
      .square(size)
      .strokeWidth(3)
      .strokeColor(Color.grey)

  /**Create Frame(Canvas/Window) to write to*/
  private val frame: Frame  = Frame.default
    .withTitle("Conways Game of Life Doodle")
    .withSize(screenWidth, screenHeight)
    .withBackground(Color.white)

  /**Calculate Cell Resolution to match screen size and cell count*/
  private def calculateResolution(): Int = {
    val resX = screenWidth / cellCountX
    val resY = screenHeight / cellCountY
    if(resX > resY) resY else resX
  }

  /**Generated Grid filled randomly with percentage $filledPercentage*/
  private def randomGrid(): Array[Array[Int]] ={
    //Create Grid Array
    val newGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    //Fill with random Black-1 and White-0 by percentage Black/White
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      newGrid(i)(j) = if(Random.between(0, 100) > filledPercentage) 1 else 0
    }
    newGrid
  }

  /**Load Grid from a file*/
  private def loadGrid(filename: String): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(filename)
    val input: Array[String] = bufferedSource.getLines.toArray
    bufferedSource.close
    val size = input(0).split(" ")
    cellCountX = size(0).toInt
    cellCountY = size(1).toInt
    resolution = calculateResolution()
    var loadedGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountY, cellCountX)
    for( i <- 1 until input.length){
      loadedGrid(i-1) = input(i).chars().map(c => c - '0').toArray
    }
    var transposedGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      transposedGrid(i)(j) = loadedGrid(j)(i)
    }
    var reversedColumnGrid: Array[Array[Int]] = Array.ofDim[Int](cellCountX, cellCountY)
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      reversedColumnGrid(i)(j) = transposedGrid(i)(cellCountY - 1 - j)
    }
    reversedColumnGrid
  }

  /**Calculates number of neighbors of a cell
    Using wraparound*/
  private def calculateNeighbor(x:Int,y:Int): Int = {
    var sum = 0
    for (i <- -1 until 2; j <- -1 until 2) {
      val row = (x + i + cellCountX) % cellCountX
      val col = (y + j + cellCountY) % cellCountY
      sum += currentGrid(row)(col)
    }
    sum -= currentGrid(x)(y)
    sum
  }

  /**Calculate new grid based on $currentGrid
   * according to rules at
   * https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life*/
  private def calculateNewGrid(): Array[Array[Int]] ={
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

  /**Update,draw and output a picture of the state of the grid*/
  private def getGridPictureAndUpdateGrid(): Picture[Unit] = {
    currentGrid = calculateNewGrid()
    val origin = Picture.circle(1).at(screenWidth, screenHeight)
    var vertices = origin
    for (i <- 0 until cellCountX; j <- 0 until cellCountY) {
      vertices = cell(resolution)
        .fillColor(if (currentGrid(i)(j) == 1) Color.black else Color.white)
        .at((i + 0.5) * resolution, (j + 0.5) * resolution)
        .on(vertices)
    }
    vertices
  }

  /** Animation Stream of Grid Pitures*/
  private val animation: Stream[IO, Picture[Unit]] = {
    Stream(1).repeat
      .debounce[IO](Duration(frameTime, TimeUnit.MILLISECONDS))
      .map { s => getGridPictureAndUpdateGrid() }
  }

  /**Animation output to frame/window*/
  def go(): Unit =
    animation.animateFrames(frame)

  /**Animation output to file*/
  def write(): Unit =
    animation.take(fileFrameCount).write[Gif]("conway.gif", frame)

}
