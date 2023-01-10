package lyra

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import org.scalajs.dom._
import scala.scalajs.js
import java.util.Optional

def isOnLine(lineStart: Point, lineEnd: Point, target: Point): Boolean = {
   lineStart.x <= target.x && target.x <= lineEnd.x &&
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

  def asPoints: List[Point] = List(topLeft, topRight, bottomRight, bottomLeft)

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
  def draw(gfx: CanvasRenderingContext2D, rect: Rectangle): Unit = {
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
  val id: String
  val user: String
  def draw(canvas: HTMLCanvasElement): Unit = {
    getGFX(canvas).stroke()
  }
  
  def addPaths(gfx: CanvasRenderingContext2D): Unit
  def applyStyles(gfx: CanvasRenderingContext2D): Unit
  def getGFX(canvas: HTMLCanvasElement): CanvasRenderingContext2D = {
    val gfx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    addPaths(gfx)
    applyStyles(gfx)
    gfx
  }
  def highlights: List[Point]
  def overlap(r: Rectangle): Boolean


sealed trait Shape[+T <: Shape[T]] extends StaticShape:
  def patchStyles(newStyles: StylesConfig): Shape[T]
  def move(d: Delta): Shape[T]



sealed trait ModifiableShape[+T <: ModifiableShape[T]] extends Shape[T] {
  def modify(p: Point): ModifiableShape[_]
}

case class StrokeShape(
    id: String, user: String,
    contents: List[Point],
    styles: StylesConfig
) extends ModifiableShape[StrokeShape] {
  
  override def patchStyles(newStyles: StylesConfig): StrokeShape =
    StrokeShape(id, user, contents, newStyles)
    
  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.color
    gfx.lineWidth = styles.lineWidth
    gfx.globalAlpha = styles.opacity
  }

  def highlights: List[Point] = {
    val interval = 10
    // select points with a interval, so that highlight points are selected
    contents.zipWithIndex
      .filter((_, i) => i % interval == 0 || i == contents.length - 1)
      .map((p, _) => p)
  }

  def overlap(r: Rectangle): Boolean = {
    // check if each point in the contents is contained in the rectangle
    contents.forall(r.contains)
  }

  override def addPaths(gfx: CanvasRenderingContext2D): Unit = {
    gfx.beginPath()
    for (p <- contents) {
      gfx.lineTo(p.x, p.y)
    }
  }

  def move(d: Delta): StrokeShape = {
    // move each point with delta
    StrokeShape(id, user, contents.map(p => p.move(d)), styles)
  }

  def modify(newPoint: Point): StrokeShape = {
    StrokeShape(id, user, this.contents :+ newPoint, styles)
  }
}

case class SelectionRectShape(id: String, user: String, val rect: Rectangle, styles: StylesConfig)
  extends Shape[SelectionRectShape] {

  override def patchStyles(newStyles: StylesConfig): SelectionRectShape =
    SelectionRectShape(id, user, rect, styles)
  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.selectionColor
    gfx.globalAlpha = styles.opacity
    gfx.lineWidth = styles.lineWidth
    // gfx.setLineDash(js.Array(5.0, 15.0))
  }

  override def highlights: List[Point] = rect.asPoints
  override def overlap(r: Rectangle): Boolean =
    rect.asPoints.forall(rect.contains)

  override def move(d: Delta): SelectionRectShape = d match {
    case Delta(dx, dy) =>
      rect match {
        case Rectangle(x, y, w, h) =>
          SelectionRectShape(id, user, Rectangle(x + dx, y + dy, w, h), styles)
      }
  }

  override def addPaths(gfx: CanvasRenderingContext2D): Unit = {
    gfx.beginPath()
    Rectangle.draw(gfx, rect)
  }


}
case class EndpointsHighlight(id: String, user: String, contents: List[Point], styles: StylesConfig)
    extends Shape[EndpointsHighlight] {

  override def patchStyles(newStyles: StylesConfig): EndpointsHighlight =
      EndpointsHighlight(id, user, contents, newStyles)


  override def move(d: Delta): EndpointsHighlight = {
    EndpointsHighlight(id, user, contents.map((p) => p.move(d)), styles)
  }
    
  override def applyStyles(gfx: CanvasRenderingContext2D): Unit = {
    gfx.strokeStyle = styles.selectionPointColor
    gfx.fillStyle = styles.selectionColor
  }

  override def addPaths(gfx: CanvasRenderingContext2D): Unit = {
    for (origin <- contents) {
      gfx.beginPath()
      // Rectangle.draw(gfx, squareInsideCircle(origin, 0.2))
      gfx.arc(origin.x, origin.y, 3, 0, 2 * Math.PI)
      gfx.closePath()
      gfx.fill()
    }
  }

  override def overlap(r: Rectangle): Boolean = contents.forall(r.contains)

  override def highlights: List[Point] = highlights
}


  implicit val pointDecoder: Decoder[Point] = deriveDecoder
  implicit val pointEncoder: Encoder[Point] = deriveEncoder

  implicit val rectangleDecoder: Decoder[Rectangle] = deriveDecoder
  implicit val rectangleEncoder: Encoder[Rectangle] = deriveEncoder

  implicit val deltaDecoder: Decoder[Delta] = deriveDecoder
  implicit val deltaEncoder: Encoder[Delta] = deriveEncoder

  implicit val selectionRectShapeDecoder: Decoder[SelectionRectShape] = deriveDecoder
  implicit val selectionRectShapeEncoder: Encoder[SelectionRectShape] = deriveEncoder

  implicit val strokeShapeDecoder: Decoder[StrokeShape] = deriveDecoder
  implicit val strokeShapeEncoder: Encoder[StrokeShape] = deriveEncoder

  implicit val endpointsHighlightDecoder: Decoder[EndpointsHighlight] = deriveDecoder
  implicit val endpointsHighlightEncoder: Encoder[EndpointsHighlight] = deriveEncoder

  implicit val shapeDecoder: Decoder[Shape[_]] = deriveDecoder
  implicit val shapeEncoder: Encoder[Shape[_]] = deriveEncoder
  implicit val mshapeDecoder: Decoder[ModifiableShape[_]] = deriveDecoder
  implicit val mshapeEncoder: Encoder[ModifiableShape[_]] = deriveEncoder