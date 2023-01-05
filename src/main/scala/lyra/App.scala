package lyra

import org.scalajs.dom

import java.util.Optional

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  private var data: List[Shape] = initialData
  private var curEditShape: Option[ModifiableShape] = None
  private val mode: AppMode = new StrokeCreateMode(this)
  canvas.onmouseup = e => mode.onMouseUp(e)
  canvas.onmousedown = e => mode.onMouseDown(e)
  canvas.onmousemove = e => mode.onMouseMove(e)

  def dataSetter(changeFn: List[Shape] => List[Shape]): Unit = {
    // now we know when a data is changed
    // we can update all the dependencies here!!!
    data = changeFn(data)
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val rect = canvas.getBoundingClientRect()
    // clear the canvas and draw again
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
    for (shape <- data) {
      shape.draw(gfx)
    }
  }

  def getCurEdit: Option[Shape] = {
    curEditShape
  }
  def isCurEdit: Boolean = {
    curEditShape match {
      case Some(_) => true
      case _ => false
    }
  }
  def setCurEditShape(shape: Option[ModifiableShape]): Unit = {
    curEditShape = shape
  }
  def modifyCurEditShape(point: Point): Unit = {
    curEditShape match {
      case Some(shape) => curEditShape = Some(shape.modify(point))
      case _ =>
    }
  }
  def clickToPoint(e: dom.MouseEvent): Point = {
    val boundingRec = canvas.getBoundingClientRect()
    val x = e.clientX - boundingRec.left
    val y = e.clientY - boundingRec.top
    Point(x, y)
  }
}


