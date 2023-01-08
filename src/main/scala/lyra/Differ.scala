package lyra

// here the there is no single position for the

// we need to check the context,
// define context: it should be same position....

trait Differ[T <: DiffableShape[T]] { Self: T =>
  def generate(oldShapes: List[T], newShapes: List[T]): Unit = {
    val oldMap = oldShapes.map(s => (s.id, s)).toMap
    val newMap = newShapes.map(s => (s.id, s)).toMap
    val zipped = (oldMap.keys ++ newMap.keys).map(key => (key, oldMap.get(key), newMap.get(key)))
    zipped.map((key, copyShape, curShape) => (copyShape, curShape) match {
      case (Some(cp), Some(cur)) => cur.diff(cp); true
      case (None, Some(cur)) => cur.diff(cur); true
      case (Some(cp), None) => "Removed"
      case (None, None) => "Not sure why this would happen"
    })
  }
}