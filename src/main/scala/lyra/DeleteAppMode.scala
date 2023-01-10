package lyra
import org.scalajs.dom.MouseEvent

class DeleteAppMode(app: App) extends AppMode {
  // use command control to issue commands
  // more like createappmode
  private var pathPoints: List[Point] = List()

  def selectedShapes: List[Shape[_]] = {
    if (pathPoints.isEmpty) {
      return List()
    }
    app.shapes.filter(shape => pathPoints.exists(p => shape.getGFX(app.canvas).isPointInPath(p.x, p.y)))
  }

  override def onMouseUp(e:  MouseEvent): Unit = {
    selectedShapes.foreach(shape => app.commandController.log(DeleteShapeCommand(shape)))
    clearState()
  }

  override def onMouseDown(e: MouseEvent): Unit = {
    pathPoints = pathPoints ++ List(app.clickToPoint(e))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    if (pathPoints.nonEmpty)
      pathPoints = pathPoints ++ List(app.clickToPoint(e))
  }

  override def clearState(): Unit = {
    pathPoints = List()
  }

  override def apply(setData: DataSetter): Unit = {
    val selected = selectedShapes
    setData(old => 
      old.map(cur => {
        if (selected.contains(cur)) {
          cur.patchStyles(cur.styles.copy(opacity=0.3))
        } else { cur }
      }))
  }
}