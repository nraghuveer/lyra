package lyra

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

type DataSetter= (List[Shape[_]] => List[Shape[_]]) => Unit

sealed trait ShapeCommand:
  def undo(setData: DataSetter): Unit
  def run(setData: DataSetter): Unit

case class CreateShapeCommand(shape: Shape[_]) extends ShapeCommand {
  def run(setData: DataSetter): Unit = {
    setData((old) => {
      old ++ List(shape)
    })
  }
  def undo(setData: DataSetter): Unit = {
    setData(old => old.filter(s => s != shape))
  }
}

case class DeleteShapeCommand(shape: Shape[_]) extends ShapeCommand {
  override def run(setData: DataSetter): Unit = {
    // TODO: check why the generic s != shape not working herew
    setData(old => {
      old.filter(s => s.id != shape.id)
    })
  }

  override def undo(setData: DataSetter): Unit = {
    // as if this command ever existed
    setData(old => old)
  }
}

case class MoveShapesCommand(shapes: List[Shape[_]], delta: Delta)
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

implicit val createShapeCommandDecoder: Decoder[CreateShapeCommand] = deriveDecoder
implicit val createShapeCommandEncoder: Encoder[CreateShapeCommand] = deriveEncoder
implicit val shapeCommandDecoder: Decoder[ShapeCommand] = deriveDecoder
implicit val shapeCommandEncoder: Encoder[ShapeCommand] = deriveEncoder
implicit val undoCommandDecoder: Decoder[UndoCommand] = deriveDecoder
implicit val undoCommandEncoder: Encoder[UndoCommand] = deriveEncoder
implicit val redoCommandDecoder: Decoder[RedoCommand] = deriveDecoder
implicit val redoCommandEncoder: Encoder[RedoCommand] = deriveEncoder
