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

case class MoveShapeCommand(shapes: List[Shape], delta: Delta)
    extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    // move the shapes if it is part of the given shapes with given delta
    def mover(s: Shape): Shape = {
      if (shapes.contains(s)) {
        s.move(delta)
      }
      s
    }
    setData(old => old.map(mover))
  }

  override def undo(setData: DataSetter): Unit = {
    def mover(s: Shape): Shape = {
      if (shapes.contains(s)) {
        s.move(delta.reverse())
      }
      s
    }
    setData(old => old.map(mover))
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
