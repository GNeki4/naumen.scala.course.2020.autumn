trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(ix: Int, iy: Int, val table:Table) extends Cell {
  override def toString: String = {
    val nextCell = getNextCell
    nextCell match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) =>
        if (cell.getNextCell.get == this) "cyclic"
        else cell.getNextCell.get.toString
      case Some(cell: Cell) => cell.toString
    }
  }

  def getNextCell: Option[Cell] = table.getCell(ix, iy)
}