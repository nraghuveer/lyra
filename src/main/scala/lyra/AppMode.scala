package lyra
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import scala.runtime.Static
import scala.scalajs.js

trait AppMode:
  def onMouseDown(e: dom.MouseEvent): Unit
  def onMouseUp(e: dom.MouseEvent): Unit
  def onMouseMove(e: dom.MouseEvent): Unit
  def editees: List[StaticShape]
  def clearState(): Unit
  def getUniqueId: String = java.util.UUID.randomUUID().toString

trait SelectionMode[S <: Shape[S], H <: Shape[H]] extends AppMode:
  def isInSelection(point: Option[Point]): Boolean
  def selectedShapes: List[Shape[S]]
  def highlightShapes: List[Shape[H]]

// Stoke and Selection
class RectangleSelectionMode(app: App) extends SelectionMode[SelectionRectShape, EndpointsHighlight] {
  private var selectionRect: Option[Rectangle] = None
  private var selectionRectStart: Option[Point] = None
  private val dragMode = new RectangleSelectionDragMode(app, this)

  override def clearState(): Unit = {
    selectionRect = None
    selectionRectStart = None
  }
  override def isInSelection(point: Option[Point]): Boolean = {
    point match {
      case Some(p) =>
        selectionRect match {
          case Some(rect) => rect.contains(p)
          case None       => false
        }
      case None => false
    }
  }

  override def selectedShapes: List[Shape[_]] = {
    // this should have different styles
    // the idea is to highlight the start and endpoints....
    // if bounding rectangle
    selectionRect match {
      case Some(rect) =>
        app.shapes
          .filter(shape => shape.overlap(rect))
      case None => List()
    }
  }

  override def highlightShapes: List[EndpointsHighlight] = {
    selectedShapes
        .map(shape => EndpointsHighlight(getUniqueId, shape.user, shape.highlights, app.styles))
  }

  override def editees: List[Shape[_]] =
    // selection Rectangle ++ shapes bounded in this rectangle
    val rect = selectionRect match {
      case Some(rect) =>
        List(SelectionRectShape(getUniqueId, app.user, rect, app.styles.copy(lineWidth = 2)))
      case None => List()
    }
    rect ++ dragMode.editees ++ highlightShapes

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

// T is the type of the shapes that the editees return
class RectangleSelectionDragMode(app: App, selectionMode: SelectionMode[_ <: Shape[_], _ <: Shape[_]])
    extends AppMode {
  private val opacity: Double = 0.3
  private var start: Option[Point] = None
  private var dragDelta: Option[Delta] = None

  override def clearState(): Unit = {
    start = None
    dragDelta = None
  }
  override def onMouseDown(e: MouseEvent): Unit = {
    val p = app.clickToPoint(e)
    if (selectionMode.isInSelection(Some(p))) {
      start = Some(p)
    } else {
      start = None
    }
  }
  override def onMouseMove(e: MouseEvent): Unit = {
    val dragEnd = app.clickToPoint(e)
    start match {
      case Some(dragStart) =>
        dragDelta = Some(Delta.from(dragStart, dragEnd))
      case None =>
        dragDelta = None
    }
  }

  override def onMouseUp(e: MouseEvent): Unit = {
    dragDelta match {
      case Some(delta) =>
        val cmd =
          MoveShapesCommand(selectionMode.selectedShapes, delta)
        app.commandController.log(cmd)
        clearState()
        selectionMode.clearState()
      case None =>
    }
    clearState()

  }

  override def editees: List[_ <: Shape[_]] = {
    dragDelta match {
      case Some(delta) =>
        selectionMode.selectedShapes
          .map(shape => shape.move(delta))
          .map(shape => shape.patchStyles(shape.styles.copy(opacity=opacity)))
      case None => List()  
    }
  }
}

// Initial mode for any shape
abstract class CreationMode(app: App) extends AppMode {
  def newShape(): ModifiableShape[_]

  private var editee: Option[ModifiableShape[_]] = None

  override def editees: List[StaticShape] = editee match {
    case Some(shape) => List(shape)
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
  override def clearState(): Unit = {}

  override def newShape(): StrokeShape = {
    StrokeShape(getUniqueId, app.user, List(), app.styles)
  }
}
