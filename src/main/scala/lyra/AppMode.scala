package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import scala.runtime.Static

trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit;
  def onMouseUp(e: dom.MouseEvent): Unit;
  def onMouseMove(e: dom.MouseEvent): Unit;
  def editees: List[StaticShape];

// Stoke and Selection
class RectangleSelectionMode(app: App) extends AppMode {
  var selectionRect: Option[Rectangle] = None
  var selectionRectStart: Option[Point] = None

  private def selectedShapes: List[StaticShape] =
    // this should have different styles
    // the idea is to highlight the start and endpoints....
    // if bounding rectangle
    selectionRect match {
      case Some(rect) =>
        app.shapes
          .filter(shape => shape.overlap(rect))
          .map(shape => shape.highlight)
      case None => List()
    }

  def editees: List[StaticShape] =
    // selection Rectangle ++ shapes bounded in this rectangle
    val rect = selectionRect match {
      case Some(rect) =>
        List(SelectionRectShape(rect, app.styles.copy(cursor = "grab")))
      case None => List()
    }
    rect ++ selectedShapes

  def onMouseDown(e: MouseEvent): Unit = {
    // start drawing rectangle
    selectionRectStart = Some(app.clickToPoint(e))
    selectionRect = None
  }
  def onMouseUp(e: MouseEvent): Unit = {
    // support only top to bottom
    val p = app.clickToPoint(e)

    selectionRectStart match {
      case Some(start) =>
        val w = p.x - start.x
        val h = p.y - start.y
        selectionRect = Some(Rectangle(start.x, start.y, w, h))
      case None =>
    }
    selectionRectStart = None
  }
  def onMouseMove(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    selectionRectStart match {
      case Some(start) =>
        val w = p.x - start.x
        val h = p.y - start.y
        selectionRect = Some(Rectangle(start.x, start.y, w, h))
      case None =>
    }
  }
}
// Initial mode for any shape
abstract class CreationMode(app: App) extends AppMode {
  def newShape(): ModifiableShape

  private var editee: Option[ModifiableShape] = None

  override def editees: List[StaticShape] = editee match {
    case Some(shape) => List(shape.asInstanceOf[StaticShape])
    case None        => List()
  }

  override def onMouseDown(e: MouseEvent): Unit = {
    editee = Some(newShape().modify(app.clickToPoint(e)))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    editee = editee match {
      case Some(editShape) => Some(editShape.modify(app.clickToPoint(e)))
      case None            => None
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

class StrokeCreateMode(app: App) extends CreationMode(app) {
  override def newShape(): ModifiableShape = {
    StrokeShape(List(), app.styles)
  }
}
