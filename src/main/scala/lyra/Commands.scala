package lyra

type DataSetter = (List[Shape] => List[Shape]) => Unit

trait ShapeCommand:
  def undo(setData: DataSetter): Unit
  def run(setData: DataSetter): Unit

case class CreateShapeCommand(shape: Shape) extends ShapeCommand {
  def run(setData: DataSetter): Unit = {
    setData((old: List[Shape]) => {
      old ++ List[Shape](shape)
    })
  }
  def undo(setData: DataSetter): Unit = {
    setData(old => old.filter(s => s != shape))
  }
}

case class MoveShapesCommand(shapes: List[Shape], delta: Delta)
    extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    // move the shapes if it is part of the given shapes with given delta
    setData(old =>
      old.map(s => {
        if (shapes.contains(s)) {
          s.move(delta)
        } else { s }
      })
    )
  }

  override def undo(setData: DataSetter): Unit = {
    setData(old => {
      old.map(s => {
        // before checking if the shape exists in the list
        // apply the move...
        if (shapes.map(s => s.move(delta)).contains(s)) {
          s.move(delta.reverse())
        } else { s }
      })
    })
  }
}

case class UndoCommand(command: ShapeCommand) extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    command.undo(setData)
  }

  override def undo(setData: DataSetter): Unit = {
    command.run(setData)
  }
}

class RedoCommand(command: ShapeCommand) extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    command.undo(setData)
  }

  override def undo(setData: DataSetter): Unit = {
    command.run(setData)
  }
}
