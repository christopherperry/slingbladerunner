
case class Node(title: String, depth: Int)  {
  var mark: Int = 0
  override def toString = "("+ title +", "+ depth +")"
}
