import scala.collection.parallel.ParIterable

class Graph {
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

  def longestChain(): List[String] = {
    val visited: ParIterable[List[String]] = nodes.keys.par.map(key => DFS(key))
    visited.toList.sortWith(_.size > _.size).head.reverse
  }

  def DFS(start: String): List[String] = {
    println(s"Starting search from: $start")

    def DFS0(movieTitle: String, visited: List[String], accumulator: List[List[String]]): List[List[String]] = {
      val neighbours: List[String] = validNeighbors(movieTitle, visited)
      if (neighbours.isEmpty) {
        val chain = visited :: accumulator
        val chainSize = visited.size
        if (chainSize  >= 245) {
          println("Found chain: " + visited.size)
        }
        chain
      } else {
        // marks v as visited, and recursively does dfs on the neighbors
        val updatedVisited = movieTitle :: visited
        neighbours.foldLeft(accumulator)((acc: List[List[String]], neighbor: String) => DFS0(neighbor, updatedVisited, acc))
      }
    }

    val result: List[String] = DFS0(start, List(), List()).sortWith(_.size > _.size).head
    println("Longest chain size this search: " + result.size)
    result
  }

  def validNeighbors(movieTitle: String, visited: List[String]): List[String] = {
    nodes(movieTitle).filterNot(title => visited.contains(title))
  }

  override def toString = {
    def build: Iterable[String] = {
      for (key <- nodes.keys) yield "[" + key + "] -> " + nodes(key).toList
    }

    "Number of nodes: " + nodes.size + "\n" + build.toList.sorted.mkString("\n")
  }
}
