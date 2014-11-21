import org.scalatest.FunSuite

class GraphTest extends FunSuite {

  test("addEdge: should add edges") {
    val graph = new Graph()
    graph += ("a" -> "b")
    graph += ("a" -> "c")
    graph += ("a" -> "d")

    graph += ("b" -> "f")
    graph += ("f" -> "g")
    graph += ("g" -> "c")

    assert(graph.getAdjacent("a") === List("d", "c", "b"))
    assert(graph.getAdjacent("b") === List("f"))
    assert(graph.getAdjacent("f") === List("g"))
    assert(graph.getAdjacent("g") === List("c"))
  }

}
