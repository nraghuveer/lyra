package lyra

trait CommandController:
  def log(c: ShapeCommand): Unit
  def undo(): Boolean
  def redo(): Boolean
  def canUndo: Boolean
  def canRedo: Boolean

class UndoCommandController extends CommandController {
  private var changes: List[ShapeCommand] = List[ShapeCommand]()
  override def log(c: ShapeCommand): Unit = {
    changes = changes ++ List(c)
  }

  override def canUndo: Boolean = changes.nonEmpty

  override def canRedo: Boolean = false

  override def undo(): Boolean = {
    if (!canUndo) {
      return false
    }
    log(new UndoCommand(changes.last))
    true
  }

  override def redo(): Boolean = false
}

