package lyra

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

type DataSetter = (List[Shape] => List[Shape]) => Unit

sealed trait ShapeCommand:
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

case class RedoCommand(command: ShapeCommand) extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    command.undo(setData)
  }

  override def undo(setData: DataSetter): Unit = {
    command.run(setData)
  }
}

// diffcommand shuld take a diffableshape, because it doesnt make sense to have diffcommand for non-diffablehsapes
// site variance
trait DiffCommand[T <: DiffableShape[_ <: DiffableShape[_]]](selfShape: T) extends ShapeCommand:
  def checkConstraints(other: T): Boolean;



implicit val createShapeCommandDecoder: Decoder[CreateShapeCommand] = deriveDecoder[CreateShapeCommand]
implicit val createShapeCommandEncoder: Encoder[CreateShapeCommand] = deriveEncoder[CreateShapeCommand]
implicit val shapeCommandDecoder: Decoder[ShapeCommand] = deriveDecoder
implicit val shapeCommandEncoder: Encoder[ShapeCommand] = deriveEncoder
implicit val undoCommandDecoder: Decoder[UndoCommand] = deriveDecoder
implicit val undoCommandEncoder: Encoder[UndoCommand] = deriveEncoder
implicit val redoCommandDecoder: Decoder[RedoCommand] = deriveDecoder
implicit val redoCommandEncoder: Encoder[RedoCommand] = deriveEncoder
//implicit val diffCommandDecoder: Decoder[DiffCommand[T <: DiffableShape[T]]] = deriveDecoder[DiffCommand[T <: DiffableShape[T]]]
//implicit val diffCommandEncoder: Decoder[DiffCommand[T <: DiffableShape[T]]] = deriveEncoder
