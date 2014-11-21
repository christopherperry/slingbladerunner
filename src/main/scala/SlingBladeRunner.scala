import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit.NANOSECONDS

import scala.io.Source

object SlingBladeRunner {

  def main(args: Array[String]): Unit = {
    val movieList: List[String] = getMovies
    println("Parsed movie file, building graph...")

    val (graph, time1) = Graph(movieList).elapsed()
    println("Built graph, printing to file...")

    val pw = new PrintWriter(new File("graph.txt"))
    pw.println(s"Time to build graph $time1")
    pw.println(graph)
    pw.flush()
    pw.close()

    // for each node visit the node, then visit each of it's adjacent nodes. Repeat
    println("Doing DFSs on every node in the graph...")
    val (longestChain, time2) = graph.longestChain().elapsed()
    println("Longest chain found is: " + longestChain.size)

    val writer = new PrintWriter(new File("longest_chain.txt"))
    writer.println("Size of chain: " + longestChain.size)
    writer.println(s"Time to find $time2")

    for ((title: String) <- longestChain) writer.println(title)
    writer.flush()
    writer.close()
  }

  def getMovies: List[String] = {
    val source: Source = Source.fromURL(getClass.getResource("/MOVIES.TXT"))
    val movieList: List[String] = source.getLines().toList
    source.close()
    movieList
  }

  implicit class RichElapsed[A](f: => A) {

    def elapsed(): (A, String) = {
      val start = System.nanoTime()
      val res = f
      val end = System.nanoTime()

      (res, NANOSECONDS.toSeconds(end - start) + " seconds")
    }

  }
}
