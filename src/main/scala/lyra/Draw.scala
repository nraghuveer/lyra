package lyra

import org.scalajs.dom
import scala.scalajs.js

import java.util.Optional
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import scala.compiletime.ops.boolean

def isOnLine(lineStart: Point, lineEnd: Point, target: Point): Boolean = {
  return lineStart.x <= target.x && target.x <= lineEnd.x &&
    lineStart.y <= target.y && target.y <= lineEnd.y
}

case class Point(x: Double, y: Double) {
  def move(d: Delta): Point = Point(x + d.dx, y + d.dy)
}

case class Rectangle(x: Double, y: Double, w: Double, h: Double) {
  private def topLeft: Point = Point(x, y)
  private def topRight: Point = Point(x + w, y)
  private def bottomRight: Point = Point(x + w, y + h)
  private def bottomLeft: Point = Point(x, y + h)

  def asPoints = List(topLeft, topRight, bottomRight, bottomLeft)

  def contains(p: Point): Boolean = {
    val horizontal = p.x >= x && p.x <= x + w
    val vertical = p.y >= y && p.y <= y + h
    horizontal && vertical
  }
  def onContour(p: Point): Boolean = {
    isOnLine(topLeft, topRight, p) ||
    isOnLine(topRight, bottomRight, p) ||
    isOnLine(bottomRight, bottomLeft, p) ||
    isOnLine(bottomLeft, topLeft, p)
  }
}

object Rectangle {
  def draw(gfx: dom.CanvasRenderingContext2D, rect: Rectangle): Unit = {
    gfx.moveTo(rect.x, rect.y)
    gfx.lineTo(rect.x + rect.w, rect.y)
    gfx.lineTo(rect.x + rect.w, rect.y + rect.h)
    gfx.lineTo(rect.x, rect.y + rect.h)
    gfx.lineTo(rect.x, rect.y)
  }
}

case class Delta(dx: Double, dy: Double) {
  def reverse(): Delta = Delta(-dx, -dy)
  def empty(): Boolean = dx == 0 && dy == 0
}
object Delta {
  def from(p1: Point, p2: Point): Delta = new Delta(p2.x - p1.x, p2.y - p1.y)
}

trait StaticShape:
  val styles: StylesConfig
  def patchStyles(newStyles: StylesConfig): StaticShape
  def draw(canvas: dom.HTMLCanvasElement): Unit
  def applyStyles(gfx: dom.CanvasRenderingContext2D): Unit
  def getGFX(canvas: dom.HTMLCanvasElement): dom.CanvasRenderingContext2D = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    applyStyles(gfx)
    gfx
  }

trait Shape extends StaticShape:
  def overlap(r: Rectangle): Boolean
  def move(d: Delta): Shape
  def highlights: List[Point]

case class SelectionRectShape(val rect: Rectangle, styles: StylesConfig)
    extends Shape {

  override def patchStyles(newStyles: StylesConfig): StaticShape =
    SelectionRectShape(rect, styles)
  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.selectionColor
    gfx.globalAlpha = styles.opacity
    gfx.lineWidth = styles.lineWidth
    // gfx.setLineDash(js.Array(5.0, 15.0))
  }

  override def highlights: List[Point] = rect.asPoints
  override def overlap(r: Rectangle): Boolean =
    rect.asPoints.forall(rect.contains)
  override def move(d: Delta): Shape = d match {
    case Delta(dx, dy) =>
      rect match {
        case Rectangle(x, y, w, h) =>
          SelectionRectShape(Rectangle(x + dx, y + dy, w, h), styles)
      }
  }

  override def draw(canvas: HTMLCanvasElement): Unit = {
    val gfx = getGFX(canvas)
    gfx.beginPath()
    Rectangle.draw(gfx, rect)
    gfx.stroke()
  }
}

trait ModifiableShape extends Shape {
  def modify(p: Point): ModifiableShape
}

case class StrokeShape(
    private val contents: List[Point],
    val styles: StylesConfig
) extends ModifiableShape {

  override def patchStyles(newStyles: StylesConfig): StaticShape =
    StrokeShape(contents, newStyles)
  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.color
    gfx.lineWidth = styles.lineWidth
    gfx.globalAlpha = styles.opacity
  }

  def highlights: List[Point] = {
    val interval = 10
    // select points with a interval, so that highlight points are selected
    contents.zipWithIndex
      .filter((p, i) => i % interval == 0 || i == contents.length - 1)
      .map((p, i) => p)
  }

  def overlap(r: Rectangle): Boolean = {
    // check if each point in the contents is contained in the rectangle
    contents.forall(r.contains)
  }
  def draw(canvas: dom.HTMLCanvasElement): Unit = {
    val gfx = getGFX(canvas)
    gfx.beginPath()
    for (p <- contents) {
      gfx.lineTo(p.x, p.y)
    }
    gfx.stroke()
  }

  def move(d: Delta): StrokeShape = {
    // move each point with delta
    StrokeShape(contents.map(p => p.move(d)), styles)
  }

  def modify(newPoint: Point): StrokeShape = {
    StrokeShape(this.contents :+ newPoint, styles)
  }
}

case class EndpointsHightlight(contents: List[Point], styles: StylesConfig)
    extends Shape {
  private val radius = 4.0

  override def patchStyles(newStyles: StylesConfig): StaticShape =
    EndpointsHightlight(contents, newStyles)
  private def squareInsideCircle(origin: Point, radius: Double): Rectangle = {
    val sideLength = Math.sqrt(2) * radius
    val topLeft = Point(origin.x - sideLength / 2, origin.y - sideLength / 2)
    Rectangle(topLeft.x, topLeft.y, sideLength, sideLength)
  }

  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.selectionPointColor
    gfx.fillStyle = styles.selectionColor
  }

  override def draw(canvas: HTMLCanvasElement): Unit = {
    val gfx = getGFX(canvas)
    for (origin <- contents) {
      gfx.beginPath()
      // Rectangle.draw(gfx, squareInsideCircle(origin, 0.2))
      gfx.arc(origin.x, origin.y, 3, 0, 2 * Math.PI)
      gfx.closePath()
      gfx.fill()
      gfx.stroke()
    }
  }

  override def move(d: Delta): Shape = {
    EndpointsHightlight(contents.map(p => p.move(d)), styles)
  }

  override def overlap(r: Rectangle): Boolean = contents.forall(r.contains)

  override def highlights: List[Point] = highlights
}
