package lyra
import org.scalajs.dom.MouseEvent

class DeleteAppMode(app: App, setData: DataSetter) extends AppMode {
  // use command control to issue commands
  // more like createappmode
  private var pathPoints: List[Point] = List()

  def selectedShapes: List[Shape[_]] = {
    app.shapes.filter(shape => pathPoints.exists(p => shape.contains(p)))
  }

  override def onMouseUp(e:  MouseEvent): Unit = {
    // do work => command controll
    println(selectedShapes)
    selectedShapes.foreach(shape => app.commandController.log(DeleteShapeCommand(shape)))
    clearState()
  }

  override def onMouseDown(e: MouseEvent): Unit = {
    pathPoints = pathPoints ++ List(app.clickToPoint(e))
  }

  override def onMouseMove(e: MouseEvent): Unit = {
    pathPoints = pathPoints ++ List(app.clickToPoint(e))
    val selected = selectedShapes
    setData(old => old.map(shape => {
      if (selected.contains(shape)) { shape.patchStyles(shape.styles.copy(opacity=0.3)) }
      else { shape }
    }))
  }

  override def clearState(): Unit = {
    pathPoints = List()
  }

  override def editees: List[StaticShape] = List()
}