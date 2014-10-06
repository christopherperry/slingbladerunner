import java.io.{File, PrintWriter}

import scala.io.Source

object SlingBladeRunner {

  def main(args: Array[String]): Unit = {
    val movieList: List[String] = getMovies
    println("Parsed movie file, building graph...")

    val graph = new GraphBuilder().build(movieList)
    println("Built graph, printing to file...")
    val pw = new PrintWriter(new File("graph.txt"))
    pw.println(graph)
    pw.flush()
    pw.close()

    // for each node visit the node, then visit each of it's adjacent nodes. Repeat
    println("Doing DFSs on every node in the graph...")
    val allDFSs: List[List[Node]] = graph.allDFS()

    println("Finished all DFSs, now onto finding the longest chain...")
    val allLongestChains: List[List[String]] = allDFSs.par.map(dfs => graph.longestChain(dfs)).toList

    val longestChainsSorted: List[List[String]] = allLongestChains.sortWith((lt, rt) => lt.size > rt.size)

    println("Longest chain found is: " + longestChainsSorted.head.size)
    val writer = new PrintWriter(new File("longest_chain.txt"))
    writer.println("Size of chain: " + longestChainsSorted.head.size)

    for ((title: String) <- longestChainsSorted.head) writer.println(title)

    writer.flush()
    writer.close()
  }

  def getMovies: List[String] = {
    val source: Source = Source.fromURL(getClass.getResource("/MOVIES.TXT"))
    val movieList: List[String] = source.getLines().toList
    source.close()
    movieList
  }
}
