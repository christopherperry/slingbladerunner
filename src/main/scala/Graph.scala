
class Graph {
  implicit def stringWrapper(movieTitle: String) = new ChainHelper(movieTitle)

  private val nodes = scala.collection.mutable.Map[String, List[String]]()

  def add(s: String) = {
    nodes += (s -> List())
  }

  def +=(kv: (String, String)) = {
    if (nodes contains kv._1) nodes += (kv._1 -> (kv._2 :: nodes(kv._1)))
    else nodes += (kv._1 -> (kv._2 :: Nil))
  }

  def getAdjacent(key: String): List[String] = {
    if (nodes contains key) nodes.apply(key)
    else Nil
  }

  /**
   * This gives the results of all DFSs, sorted by
   * size of the resulting list
   *
   * IDEA: Search each DFS with some sort of look-ahead
   * algorithm that builds out all the chains within each DFS.
   * Do this for each DFS, then compare the longest chain from each.
   *
   */
  def allDFS(): List[List[Node]] = {
    // for each key, traverse adjacent
    val visited: Iterable[List[Node]] = for (key <- nodes.keys) yield DFS(key)
    for (list <- visited) println("DFS visited length: " + list.size)

    visited.toList.sortBy(_.size)
  }

  def DFS(start: String): List[Node] = {

    def DFS0(v: String, visited: List[Node], depth: Int): List[Node] = {
      if (visited.contains(v))
        visited
      else {
        println("Checking value: " + v)
        val newDepth = depth + 1
        val neighbours: List[String] = nodes(v) filterNot (title => visited.exists(node => node.title == title))

        // marks v as visited, and recursively does dfs on the neighbors
        neighbours.foldLeft(Node(v, newDepth) :: visited)((b: List[Node], a: String) => DFS0(a, b, newDepth))
      }
    }
    DFS0(start, List(), 0)
  }

//  def DFS(start: String): List[Node] = {
//    val white = 0
//    val grey = 1
//    val black = 2
//
//    def visit(node: Node, acc: List[Node], depth: Int): List[Node] = {
//      if (node.mark != white) acc
//      else {
//        node.mark = grey
//        val neighbors = nodes(node.title)
//
//        val newDepth = depth + 1
//        for (neighbor <- neighbors) visit(Node(neighbor, newDepth), acc, newDepth)
//        node.mark = black
//        node :: acc
//      }
//    }
//
//    visit(Node(start, 0), List(), 0)
//  }

  /**
   * The idea here is to sort the DFS tree by depth
   * making sure to back up through matches on
   * the previous movie title. This should, in theory,
   * give the longest chain in the DFS.
   */
  def longestChain(dfs: List[Node]): List[String] = {
    val sorted: List[Node] = dfs.sortWith((lt, rt) => lt.depth > rt.depth)

    def linkChain(node: Node, nodes: List[Node], acc: List[String]): List[String] = {
      if (nodes.isEmpty) node.title :: acc
      else {
        val head: Node = nodes.head

        if (node.depth == (head.depth + 1) && (head.title canChain node.title))
          linkChain(head, nodes.tail, node.title :: acc)
        else linkChain(node, nodes.tail, acc)

      }
    }

    linkChain(sorted.head, sorted.tail, Nil)
  }

  override def toString = {
    def build: Iterable[String] = {
      for (key <- nodes.keys) yield "[" + key + "] -> " + nodes(key).toList
    }

    "Number of nodes: " + nodes.size + "\n" + build.toList.sorted.mkString("\n")
  }
}
