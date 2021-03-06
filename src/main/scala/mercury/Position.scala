package mercury

// Represnts a position on a promotion page
case class Position(
  src: Page,
  component: String,
  idx: Option[Int] = None,
  sublinkIdx: Option[Int] = None
) {
  def inWords =
    s"""${src.name} "$component" position ${idx.getOrElse("?")}""" +
      sublinkIdx.map(" sublink " + _).getOrElse("")

  def inWordsWithoutPageName =
    s""""$component" position ${idx.getOrElse("?")}""" +
      sublinkIdx.map(" sublink " + _).getOrElse("")

  def isSublink = sublinkIdx.isDefined
}

object Position {
  implicit val defaultOrdering: Ordering[Position] = Ordering.by(p => (p.component, p.idx, p.sublinkIdx))
}
