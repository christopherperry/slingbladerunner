import scala.collection.parallel.ParIterable

object Graph {
  implicit def stringWrapper(movieTitle: String) = new ChainHelper(movieTitle)

  def build(movieTitles: List[String]): Map[String, List[String]] = {
    movieTitles.map(key => key -> buildValues(key, movieTitles)).toMap
  }

  def buildValues(key: String, values: List[String]): List[String] = {
    def buildValues(key: String, values: List[String], acc: List[String]): List[String] = {
      if (values.isEmpty) acc
      else {
        if (key canChain values.head) buildValues(key, values.tail, values.head :: acc)
        else buildValues(key, values.tail, acc)
      }
    }

    buildValues(key, values, List())
  }

  def apply(movieTitles: List[String]) = {
    new Graph(build(movieTitles))
  }
}

class Graph(map: Map[String, List[String]]) {

  def longestChain(): List[String] = {
    val visited: ParIterable[List[String]] = map.keys.par.map(key => DFS(key))
    visited.toList.sortWith(_.size > _.size).head.reverse
  }

  def DFS(start: String): List[String] = {
    println(s"Starting search from: $start")

    def DFS0(movieTitle: String, visited: List[String], accumulator: List[List[String]]): List[List[String]] = {
      val neighbours: List[String] = validNeighbors(movieTitle, visited)
      if (neighbours.isEmpty) {
        val chain = visited :: accumulator
        val chainSize = visited.size
        if (chainSize  >= 230) {
          println(s"Found chain: $chainSize")
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
    map(movieTitle).filterNot(title => visited.contains(title))
  }

  override def toString = {
    def build: Iterable[String] = {
      for (key <- map.keys) yield "[" + key + "] -> " + map(key).toList
    }

    "Number of nodes: " + map.size + "\n" + build.toList.sorted.mkString("\n")
  }
}
