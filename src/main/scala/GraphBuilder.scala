
class GraphBuilder {
  implicit def stringWrapper(movieTitle: String) = new ChainHelper(movieTitle)

  def build(movieTitles: List[String]): Graph = {
    val graph = new Graph()

    for (key <- movieTitles) {
      graph.add(key)
      for (value <- movieTitles)
        if (key canChain value) graph += (key -> value)
    }
    graph
  }
}
