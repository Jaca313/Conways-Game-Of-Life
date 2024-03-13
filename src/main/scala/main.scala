import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.core.format.*
import doodle.effect.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import doodle.syntax.all.*
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.DurationInt
import scala.util.Random


@main
def main(): Unit = {
    ConwayAnimation.write()
    ConwayAnimation.go()
}

