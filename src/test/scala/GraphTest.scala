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

  test("longestChain: should construct longest chain by depth values and title match") {
    val list = List(
      Node("the big", 1),
      Node("pirates of the caribbean", 5),
      Node("big dog", 2),
      Node("aaa", 2),
      Node("bbb", 2),
      Node("dog the bounty hunter", 3),
      Node("ccc", 3),
      Node("ddd", 4),
      Node("bounty hunter pirates", 4),
      Node("eee", 4)
    )

    val chain: List[String] = new Graph().longestChain(list)
    assert(chain === List(
      "the big",
      "big dog",
      "dog the bounty hunter",
      "bounty hunter pirates",
      "pirates of the caribbean"
    ))
  }
}
