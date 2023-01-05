package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent


trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit;
  def onMouseUp(e: dom.MouseEvent): Unit;
  def onMouseMove(e: dom.MouseEvent): Unit;
  def curEditee: Option[Shape]; // so that they get a immutable type, so they cannot change it

// Stoke and Selection

// Initial mode for any shape
abstract class ShapeCreatorMode(app: App) extends AppMode {
  def newShape(): ModifiableShape

  private var editee: Option[ModifiableShape] = None

  override def curEditee: Option[Shape] = editee

  override def onMouseDown(e: MouseEvent): Unit = {
    editee = Some(newShape().modify(app.clickToPoint(e)))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    editee = editee match {
      case Some(editShape) => Some(editShape.modify(app.clickToPoint(e)))
      case None => None
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    editee = editee match {
      case Some(editShape) =>
        app.commandController.log(CreateShapeCommand(editShape))
        None
      case None => None
    }
  }
}

class StrokeCreateMode(app: App) extends ShapeCreatorMode(app) {
  override def newShape(): ModifiableShape = {
    StrokeShape(List(), app.styles)
  }
}

