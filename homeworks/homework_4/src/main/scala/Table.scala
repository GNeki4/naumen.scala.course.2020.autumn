import scala.collection.mutable

class Table(x: Int, y: Int) {
  private val cells = mutable.Map[Int, Cell]().withDefault(_ => new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    val index = ix + iy * x
    if (ix >= 0 && ix < x && iy >= 0 && iy < y)
      Option(cells(index))
    else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    val index = ix + iy * x
    cells(index) = cell
  }
}