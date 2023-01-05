package lyra

import org.scalajs.dom

import java.util.Optional

class App(canvas: dom.HTMLCanvasElement, initialData: List[Shape]) {
  private var data: List[Shape] = initialData
  private val mode: AppMode = new StrokeCreateMode(this)
  val commandController = new UndoCommandController()
  canvas.onmouseup = e => mode.onMouseUp(e)
  canvas.onmousedown = e => mode.onMouseDown(e)
  canvas.onmousemove = e => mode.onMouseMove(e)

  private def paint(gfx: dom.CanvasRenderingContext2D): Unit = {
    val rect = canvas.getBoundingClientRect()
    gfx.clearRect(rect.x, rect.y, rect.width, rect.height)
    data = List()
    commandController.run(this.dataSetter)
    for (shape <- data) {
      shape.draw(gfx)
    }
  }

  def dataSetter(changeFn: List[Shape] => List[Shape]): Unit = {
    data = changeFn(data)
  }

  def clickToPoint(e: dom.MouseEvent): Point = {
    val boundingRec = canvas.getBoundingClientRect()
    val x = e.clientX - boundingRec.left
    val y = e.clientY - boundingRec.top
    Point(x, y)
  }
}


