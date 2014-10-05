import scala.io.Source

object SlingBladeRunner {

  def main(args: Array[String]): Unit = {
    val movieList: List[String] = getMovies
    println("Parsed movie file, building graph...")

    val graph = new GraphBuilder().build(movieList)
    println("Built graph:")
    println(graph)

    // for each node visit the node, then visit each of it's adjacent nodes. Repeat
    println("Longest chain is: \n" + graph.longestDFS())
  }

  def getMovies: List[String] = {
    val source: Source = Source.fromURL(getClass.getResource("/MOVIES.TXT"))
    val movieList: List[String] = source.getLines().toList
    source.close()
    movieList
  }
}
