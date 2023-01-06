package lyra

import org.scalajs.dom

import java.util.Optional
import org.scalajs.dom.HTMLCanvasElement

case class Point(x: Double, y: Double) {
  def move(d: Delta): Point = Point(x + d.dx, y + d.dy)
}

case class Rectangle(x: Double, y: Double, w: Double, h: Double) {
  def contains(p: Point): Boolean = {
    val horizontal = p.x >= x && p.x <= x + w
    val vertical = p.y >= y && p.y <= y + h
    horizontal && vertical
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
}
object Delta {
  def from(p1: Point, p2: Point): Delta = new Delta(p2.x - p1.x, p2.y - p1.y)
}

trait StaticShape:
  def draw(canvas: dom.HTMLCanvasElement): Unit

trait HighlightableShape extends StaticShape:
  def highlight: StaticShape;

trait Shape extends HighlightableShape:
  def overlap(r: Rectangle): Boolean
  def move(d: Delta): ModifiableShape

case class SelectionRectShape(val rect: Rectangle, styles: StylesConfig)
    extends StaticShape {
  override def draw(canvas: HTMLCanvasElement): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    gfx.strokeStyle = styles.selectionColor
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
    private val styles: StylesConfig
) extends ModifiableShape {

  def highlight: StaticShape = {
    val interval = 10
    // select points with a interval, so that highlight points are selected
    val highlights = contents.zipWithIndex
      .filter((p, i) => i % interval == 0 || i == contents.length - 1)
      .map((p, i) => p)
    EndpointsHightlight(highlights, styles)
  }

  def overlap(r: Rectangle): Boolean = {
    // check if each point in the contents is contained in the rectangle
    contents.forall(r.contains)
  }
  def draw(canvas: dom.HTMLCanvasElement): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    gfx.strokeStyle = styles.color
    gfx.lineWidth = styles.lineWidth
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
    extends StaticShape {
  private val radius = 4.0

  private def squareInsideCircle(origin: Point, radius: Double): Rectangle = {
    val sideLength = Math.sqrt(2) * radius
    val topLeft = Point(origin.x - sideLength / 2, origin.y - sideLength / 2)
    Rectangle(topLeft.x, topLeft.y, sideLength, sideLength)
  }

  override def draw(canvas: HTMLCanvasElement): Unit = {
    val gfx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    gfx.strokeStyle = styles.selectionPointColor
    gfx.fillStyle = styles.selectionColor
    for (origin <- contents) {
      gfx.beginPath()
      // Rectangle.draw(gfx, squareInsideCircle(origin, 0.2))
      gfx.arc(origin.x, origin.y, 3, 0, 2 * Math.PI)
      gfx.closePath()
      gfx.fill()
      gfx.stroke()
    }
  }
}
