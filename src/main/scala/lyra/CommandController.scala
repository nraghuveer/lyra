package lyra
import org.scalajs.dom
import scala.collection.mutable
import io.circe.syntax._

trait CommandController[T <: Shape[T]]:
  def log(c: ShapeCommand[T]): Unit
  def undo(): Boolean
  def redo(): Boolean
  def run(setData: DataSetter[T]): Unit

class UndoRedoCommandController[T <: Shape[T]](private var changes: List[ShapeCommand[T]] = List()) extends CommandController[T] {
  private val undoStack: mutable.Stack[ShapeCommand[T]] = mutable.Stack()
  private val redoStack: mutable.Stack[UndoCommand[T]] = mutable.Stack()

  override def run(setData: DataSetter[T]): Unit = {
    for (cmd <- changes) {
      cmd.run(setData)
    }
  }
  override def log(c: ShapeCommand[T]): Unit = {
    changes = changes ++ List(c)
    // add to undo stack only if it is not undo command, else we endup in loop
    // where only last shape will be undoed
    c match {
      case uc: UndoCommand[T] => redoStack.push(uc)
      case rc: RedoCommand[T] => undoStack.push(rc)
      // if there is any other shape otherthan undocommand
      // reset the redostack
      case _: ShapeCommand[T] =>
        undoStack.push(c)
        redoStack.popAll()
    }
  }

  override def undo(): Boolean = {
    if (undoStack.isEmpty) { return false }
    log(UndoCommand(undoStack.pop()))
    true
  }

  override def redo(): Boolean = {
    if (redoStack.isEmpty) { return false }
    log(RedoCommand(redoStack.pop()))
    true
  }
}
