
class Graph {
  implicit def stringWrapper(movieTitle: String) = new ChainHelper(movieTitle)

  private val nodes = scala.collection.mutable.Map[String, List[String]]()

  def add(s: String) = {
    nodes += (s -> List())
  }

  def +=(kv: (String, String)) = {
    if (nodes contains kv._1) nodes += (kv._1 -> (kv._2 :: nodes(kv._1)))
    else nodes += (kv._1 -> (kv._2 :: Nil))
    this
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

    visited.toList.sortBy(list => list.size)
  }

  /**
   * The idea here is to reverse the DFS
   * making sure to back up through matches on
   * the previous movie title. This should, in theory,
   * give the longest chain in the DFS.
   * TODO: update with backtracking based on depth tag of nodes
   */
  //  def reverseDFS(dfs: List[String]): List[String] = {
  //    def reverseDFS0(dec: List[String], acc: List[String]): List[String] = {
  //      if (dec.isEmpty) acc
  //      else {
  //        val head: String = dec.head
  //        val tail: List[String] = dec.tail
  //
  //        if (tail.isEmpty) head :: acc
  //        else {
  //          if (tail.head canChain head) reverseDFS0(tail.tail, head :: tail.head :: acc)
  //          else reverseDFS0(tail.tail, acc)
  //        }
  //      }
  //    }
  //
  //    val reverse: List[String] = dfs.reverse
  //    reverseDFS0(reverse, List())
  //  }

  def DFS(start: String): List[Node] = {

    def DFS0(v: String, visited: List[Node], depth: Int): List[Node] = {
      if (visited.contains(v))
        visited
      else {
        println("Checking value: " + v)
        val newDepth = depth + 1
        val neighbours: List[String] = nodes(v) filterNot (s => visited.foldLeft(false)((b, n) => b || n.title == s))

        // marks v as visited, and recursively does dfs on the neighbors
        neighbours.foldLeft(Node(v, newDepth) :: visited)((b: List[Node], a: String) => DFS0(a, b, newDepth))
      }
    }
    DFS0(start, List(), 0)
  }

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
