@main
def main(): Unit = {
    val conway = ConwayAnimation.default
      .withScreenSize(1200,1200)
      .withCellDimensions(20,20)
      .withInputRandom()
//      .withInputFromFile("conway.txt")

    conway.write()
    conway.go()
}

