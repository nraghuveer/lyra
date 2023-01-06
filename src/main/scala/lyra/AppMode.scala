package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import scala.runtime.Static

trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit;
  def onMouseUp(e: dom.MouseEvent): Unit;
  def onMouseMove(e: dom.MouseEvent): Unit;
  def editees: List[StaticShape];

trait SelectionMode extends AppMode:
  def isSelection: Boolean

// Stoke and Selection
class RectangleSelectionMode(app: App) extends SelectionMode {
  var selectionRect: Option[Rectangle] = None
  var selectionRectStart: Option[Point] = None
  private val dragMode: AppMode = new RectangleSelectionDragMode(this)

  override def isSelection: Boolean = selectionRect match {
    case Some(_) => true
    case None    => false
  }

  def isOnSelection(point: Option[Point]): Boolean = {
    point match {
      case Some(p) =>
        selectionRect match {
          case Some(rect) => rect.contains(p)
          case None       => false
        }
      case None => false
    }
  }

  def isInSelection(point: Option[Point]): Boolean = {
    point match {
      case Some(p) =>
        selectionRect match {
          case Some(rect) => rect.contains(p)
          case None       => false
        }
      case None => false
    }
  }

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
        List(SelectionRectShape(rect, app.styles))
      case None => List()
    }
    rect ++ selectedShapes

  def onMouseDown(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    // clear only if the point is not IN selection
    if (!isInSelection(Some(p))) {
      selectionRectStart = Some(app.clickToPoint(e))
      selectionRect = None
    }
    dragMode.onMouseDown(e)
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
    dragMode.onMouseUp(e)
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
    dragMode.onMouseMove(e)
  }
}

class RectangleSelectionDragMode(selectionMode: SelectionMode) extends AppMode {
  private var dragDelta: Option[Delta] = None
  private val opacity: Double = 0.4

  override def onMouseDown(e: MouseEvent): Unit = {}
  override def onMouseMove(e: MouseEvent): Unit = {
    if (!selectionMode.isSelection)
      return
  }
  override def onMouseUp(e: MouseEvent): Unit = {}
  override def editees: List[StaticShape] = List()
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
