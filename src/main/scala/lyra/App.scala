package lyra

import org.scalajs.dom

import java.util.Optional

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  private var data: List[Shape] = initialData
  private var editee: Option[ModifiableShape] = None
  private val mode: AppMode = new StrokeCreateMode(this)
  canvas.onmouseup = e => mode.onMouseUp(e)
  canvas.onmousedown = e => mode.onMouseDown(e)
  canvas.onmousemove = e => mode.onMouseMove(e)

  private def drawEverything(): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val rect = canvas.getBoundingClientRect()
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
    gfx.strokeStyle = "white"
    editee match {
      case Some(shape) => shape.draw(gfx)
      case _ =>
    }
    // clear the canvas and draw again
    gfx.strokeStyle = "white"
    for (shape <- data) {
      shape.draw(gfx)
    }
  }

  def editeeSetter(changeFn: Option[ModifiableShape] => Option[ModifiableShape]): Unit = {
    editee = changeFn(editee)
    drawEverything()
  }
  def dataSetter(changeFn: List[Shape] => List[Shape]): Unit = {
    // now we know when a data is changed
    // we can update all the dependencies here!!!
    data = changeFn(data)
    drawEverything()
  }

  def clickToPoint(e: dom.MouseEvent): Point = {
    val boundingRec = canvas.getBoundingClientRect()
    val x = e.clientX - boundingRec.left
    val y = e.clientY - boundingRec.top
    Point(x, y)
  }
}


