
class Graph {
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
   * This gives the longest DFS.
   *
   * IDEA: Search each DFS with some sort of look-ahead
   * algorithm that builds out all the chains within each DFS.
   * Do this for each DFS, then compare the longest chain from each.
   *
   */
  def longestDFS(): List[String] = {
    // for each key, traverse adjacent
    val searches: Iterable[List[String]] = for (key <- nodes.keys) yield DFS(key)
    for (list <- searches) println("Chain length: " + list.size)

    //      searches.foldLeft(0)((acc, list) => if (list.size > acc) list.size else acc)

    searches.toList.sortBy(list => list.size).reverse.head
  }

  def DFS(start: String): List[String] = {

    def DFS0(v: String, visited: List[String]): List[String] = {
      if (visited.contains(v))
        visited
      else {
        println("Checking value: " + v)
        val neighbours: List[String] = nodes(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b, a) => DFS0(a, b))
      }
    }
    DFS0(start, List()).reverse
  }

  override def toString = {
    def build: Iterable[String] = {
      for (key <- nodes.keys) yield "[" + key + "] -> " + nodes(key).toList
    }

    "Number of nodes: " + nodes.size + "\n" + build.toList.sorted.mkString("\n")
  }
}
